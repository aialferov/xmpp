%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-lp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 23 Aug 2013 by Anton I Alferov <casper@ubca-lp>
%%%-------------------------------------------------------------------

-module(xmpp_auth).

-export([make_auth/1, make_auth/2]).
-export([negotiate/3]).
-export([jid/1]).

-include("xmpp.hrl").

-include("oauth2.hrl").

-include("utils_sasl.hrl").
-include("utils_monad.hrl").

-include("../src/rfc/xmpp_core_tools.hrl").

-record(auth, {oauth, user_name, password}).

-define(Mechanisms, [
	"OAUTH",
	"X-OAUTH2",
	"X-FACEBOOK-PLATFORM",
	"X-MESSENGER-OAUTH2",
	"DIGEST-MD5",
	"PLAIN"
]).

-define(DigestMd5Config, #digest_md5_config{
	qop = "auth", charset = "utf-8", serv_type = "xmpp"}).

-define(XmppStreamError(Name, Value), #streamError{
	condition = #condition{name = Name, value = Value}}).

make_auth(OAuth) -> #auth{oauth = OAuth}.
make_auth(UserName, Password) ->
	#auth{user_name = UserName, password = Password}.

%negotiate(Network, Auth, Tcp) -> negotiate(Network, Auth, Tcp,
%	xmpp_gate:open_stream(?HostName(Network), Tcp)).
%
%login({Network, Auth}) ->
%	{ok, Tcp} = xmpp_gate:connect(?HostName(Network)),
%	{ok, {tls, Tls}} = negotiate(Network, Auth, Tcp),
%	{ok, {see_other_host, Tcp1}} = negotiate(Network, Auth, Tls),
%	{ok, {tls, Tls1}} = negotiate(Network, Auth, Tcp1),
%	{ok, {mechanisms, sasl_success}} = negotiate(Network, Auth, Tls1),
%	{ok, {features, Features}} = negotiate(Network, Auth, Tls1),
%	{ok, Jid} = jid(Features),
%	xmpp_gate:roster_get(Jid, Tls1),
%	xmpp_transport:set_active(Tls1, true);

negotiate(Network, Auth, Tcp) -> negotiate({Network, Auth, Tcp}, negotiate(
	Network, Auth, Tcp, xmpp_gate:open_stream(?HostName(Network), Tcp))).

negotiate({Network, Auth, Tcp}, Result) -> case Result of
	{ok, {tls, Tls}} -> negotiate(Network, Auth, Tls);
	{ok, {see_other_host, OtherTcp}} -> negotiate(Network, Auth, OtherTcp);
	{ok, {mechanisms, sasl_success}} -> negotiate(Network, Auth, Tcp);
	{ok, {mechanisms, SaslError}} -> {error, SaslError};
	{ok, {features, Features}} -> {ok, {Tcp, Features}};
	Error -> Error
end.

negotiate(Network, Auth, Tcp, {ok, Result}) -> negotiate(utils_monad:do(
	negotiation_funs(Network, Auth, Tcp, Result)));
negotiate(_Network, _Auth, _Tcp, Error) -> Error.

negotiate([#result{id = ID, data = undefined}|_]) -> {ok, ID};
negotiate([#result{id = sasl, data = Result}|_]) -> {ok, Result};
negotiate([#result{id = ID, data = Result}|_]) -> {ok, {ID, Result}};
negotiate([#error{reason = Reason}|_]) -> {error, Reason}.

negotiate_features(Network, Auth, Tcp, Features) -> negotiate_features(
	utils_monad:do([feature_fun(Network, Auth, Tcp, F) || F <- Features])).

negotiate_features([]) -> ok;
negotiate_features(Results = [#result{}|_]) -> {ok, Results};
negotiate_features(Results = [#error{}|_]) -> {error, Results}.

feature_fun(Network, _Auth, Tcp, Feature) -> {Name, Args} = case Feature of
	bind -> {fun xmpp_gate:bind/1, [Tcp]};
	session -> {fun xmpp_gate:establish_session/2, [?HostName(Network), Tcp]};
	Feature -> {fun() -> {ok, not_supported} end, []}
end, #function{id = Feature, name = Name, args = Args}.

negotiation_funs(_Network, _Auth, _Tcp,
	?XmppStreamError('see-other-host', {HostName, Port}))
-> [
	#function{id = see_other_host,
		name = fun xmpp_gate:connect/2, args = [HostName, Port]}
];

negotiation_funs(_Network, _Auth, _Tcp, ?XmppStreamError(Name, Value)) -> [
	#function{id = e, name = fun() -> {error, {Name, Value}} end, args = []}
];

negotiation_funs(_Network, _Auth, Tcp, {features, [Tls|_]})
	when Tls == start_tls; Tls == {start_tls, required}
-> [
	#function{id = s, name = fun xmpp_gate:start_tls/1, args = [Tcp]},
	#function{id = tls, name = fun xmpp_gate:connect_tls/1, args = [Tcp]}
];

negotiation_funs(Network, Auth, Tcp,
	{features, [{mechanisms, Mechanisms}|_]})
-> [
	#function{id = mechanisms, name = fun negotiate/4, args = [
		Network, Auth, Tcp, mechanism(Mechanisms)]}
];

negotiation_funs(Network, Auth, Tcp, {features, Features}) -> [
	#function{id = features, name = fun negotiate_features/4,
		args = [Network, Auth, Tcp, Features]}
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "OAUTH") -> [
	#function{id = sasl, name = fun xmpp_gate:begin_sasl/3, args = [
		Mechanism, utils_sasl:plain_message("=", access_token(Auth)), Tcp]}
%		Mechanism, access_token(Auth), Tcp]}
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-OAUTH2") -> [
	#function{id = sasl, name = fun xmpp_gate:begin_sasl/3, args = [
		Mechanism, utils_sasl:plain_message("=", access_token(Auth)), Tcp]}
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-MESSENGER-OAUTH2") -> [
	#function{id = sasl, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, access_token(Auth), Tcp]}
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-FACEBOOK-PLATFORM") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, "=", Tcp]},
	#function{id = sasl, name = fun({sasl_challenge, Challenge}) ->
		Q = utils_http:read_query(binary_to_list(base64:decode(Challenge))),
		xmpp_gate:send_sasl_response(
			binary_to_list(base64:encode(utils_http:query_string([
				{"nonce", utils_lists:keyfind2("nonce", Q)},
				{"method", utils_lists:keyfind2("method", Q)},
				{"api_key", client_id(Auth)},
				{"access_token", access_token(Auth)},
				{"call_id", "0"},
				{"v", "1.0"}
			]))),
		Tcp)
	end, args = [#placeholder{id = b}]}
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "PLAIN") -> [
	#function{id = begin_sasl, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, utils_sasl:plain_message(
			user_name(Auth), password(Auth)), Tcp]
	}
];

negotiation_funs(Network, Auth, Tcp, Mechanism = "DIGEST-MD5") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, "=", Tcp]},
	#function{id = r1, name = fun({sasl_challenge, Challenge}) ->
		xmpp_gate:send_sasl_response(utils_sasl:digest_md5_response(
			user_name(Auth), password(Auth), ?HostName(Network),
			Challenge, ?DigestMd5Config
		), Tcp)
	end, args = [#placeholder{id = b}]},
	#function{id = r2, name = fun xmpp_gate:send_sasl_response/2,
		args = ["", Tcp]}
].

mechanism(Mechanisms) -> mechanism(Mechanisms, ?Mechanisms).
mechanism(Mechanisms, [H|T]) -> case lists:member(H, Mechanisms) of
	true -> {ok, H}; false -> mechanism(Mechanisms, T) end;
mechanism(_Mechanisms, []) -> {error, no_supported_mechanism}.

jid(Features) -> case lists:keyfind(bind, #result.id, Features) of
	#result{data = #stanza{content = Jid}} -> {ok, Jid}; 
	false -> {error, not_found}
end.

access_token(#auth{oauth = #oauth2{token = #oauth2_token{access = T}}}) -> T.
client_id(#auth{oauth = #oauth2{client = #oauth2_client{id = ID}}}) -> ID.

user_name(#auth{user_name = UserName}) -> UserName.
password(#auth{password = Password}) -> Password.
