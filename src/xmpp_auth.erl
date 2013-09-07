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

-record(auth, {type, oauth, user_name, password}).

-define(Mechanisms(Type), case Type of
	oauth -> [
		"OAUTH",
		"X-OAUTH2",
		"X-FACEBOOK-PLATFORM",
		"X-MESSENGER-OAUTH2"
	];
	name_pass -> [
		"DIGEST-MD5",
		"PLAIN"
	]
end).

-define(DigestMd5Config, #digest_md5_config{
	qop = "auth", charset = "utf-8", serv_type = "xmpp"}).

-define(XmppStreamError(Name, Value), #streamError{
	condition = #condition{name = Name, value = Value}}).

make_auth(OAuth) -> #auth{type = oauth, oauth = OAuth}.
make_auth(UserName, Password) -> #auth{type = name_pass,
	user_name = UserName, password = Password}.

%negotiate(Network, Auth, Tcp) -> negotiate(Network, Auth, Tcp,
%	xmpp_request:open_stream(?HostName(Network), Tcp)).
%
%login({Network, Auth}) ->
%	{ok, Tcp} = xmpp_request:connect(?HostName(Network)),
%	{ok, {tls, Tls}} = negotiate(Network, Auth, Tcp),
%	{ok, {see_other_host, Tcp1}} = negotiate(Network, Auth, Tls),
%	{ok, {tls, Tls1}} = negotiate(Network, Auth, Tcp1),
%	{ok, {mechanisms, sasl_success}} = negotiate(Network, Auth, Tls1),
%	{ok, {features, Features}} = negotiate(Network, Auth, Tls1),
%	{ok, Jid} = jid(Features),
%	xmpp_request:roster_get(Jid, Tls1),
%	xmpp_transport:set_active(Tls1, true);

negotiate(Network, Auth, Tcp) -> negotiate({Network, Auth, Tcp}, negotiate(
	Network, Auth, Tcp, xmpp_request:open_stream(?HostName(Network), Tcp))).

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
	bind -> {fun xmpp_request:bind/1, [Tcp]};
	session -> {fun xmpp_request:establish_session/2,
		[?HostName(Network), Tcp]};
	Feature -> {fun() -> {ok, not_supported} end, []}
end, ?Function(Feature, Name, Args).

negotiation_funs(_Network, _Auth, _Tcp,
	?XmppStreamError('see-other-host', {HostName, Port}))
-> [
	?Function(see_other_host, fun xmpp_request:connect/2, [HostName, Port])
];

negotiation_funs(_Network, _Auth, _Tcp, ?XmppStreamError(Name, Value)) -> [
	?Function(stream_error, fun() -> {error, {Name, Value}} end, [])
];

negotiation_funs(_Network, _Auth, Tcp, {features, [Tls|_]})
	when Tls == start_tls; Tls == {start_tls, required}
-> [
	?Function(start_tls, fun xmpp_request:start_tls/1, [Tcp]),
	?Function(tls, fun xmpp_request:connect_tls/1, [Tcp])
];

negotiation_funs(Network, Auth, Tcp,
	{features, [{mechanisms, Mechanisms}|_]})
-> [
	?Function(mechanisms, fun negotiate/4,
		[Network, Auth, Tcp, mechanism(Mechanisms, Auth)])
];

negotiation_funs(Network, Auth, Tcp, {features, Features}) -> [
	?Function(features, fun negotiate_features/4,
		[Network, Auth, Tcp, Features])
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "OAUTH") -> [
	?Function(sasl, fun xmpp_request:begin_sasl/3,
		[Mechanism, access_token(Auth), Tcp])
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-OAUTH2") -> [
	?Function(sasl, fun xmpp_request:begin_sasl/3,
		[Mechanism, utils_sasl:plain_message("=", access_token(Auth)), Tcp])
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-MESSENGER-OAUTH2") -> [
	?Function(sasl, fun xmpp_request:begin_sasl/3,
		[Mechanism, access_token(Auth), Tcp])
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "X-FACEBOOK-PLATFORM") -> [
	?Function(begin_sasl, fun xmpp_request:begin_sasl/3,
		[Mechanism, "=", Tcp]),
	?Function(sasl, fun({sasl_challenge, Challenge}) ->
		Q = utils_http:read_query(binary_to_list(base64:decode(Challenge))),
		xmpp_request:send_sasl_response(
			binary_to_list(base64:encode(utils_http:query_string([
				{"nonce", utils_lists:keyfind2("nonce", Q)},
				{"method", utils_lists:keyfind2("method", Q)},
				{"api_key", client_id(Auth)},
				{"access_token", access_token(Auth)},
				{"call_id", "0"},
				{"v", "1.0"}
			]))),
		Tcp)
	end, [?Placeholder(begin_sasl)])
];

negotiation_funs(_Network, Auth, Tcp, Mechanism = "PLAIN") -> [
	?Function(sasl, fun xmpp_request:begin_sasl/3, [Mechanism,
		utils_sasl:plain_message(user_name(Auth), password(Auth)), Tcp])
];

negotiation_funs(Network, Auth, Tcp, Mechanism = "DIGEST-MD5") -> [
	?Function(begin_sasl, fun xmpp_request:begin_sasl/3,
		[Mechanism, "=", Tcp]),
	?Function(sasl_challenge, fun({sasl_challenge, Challenge}) ->
		xmpp_request:send_sasl_response(utils_sasl:digest_md5_response(
			user_name(Auth), password(Auth), ?HostName(Network),
			Challenge, ?DigestMd5Config
		), Tcp)
	end, [?Placeholder(begin_sasl)]),
	?Function(sasl, fun xmpp_request:send_sasl_response/2, ["", Tcp])
].

mechanism(Mechanisms, #auth{type = Type}) ->
	mechanism({Mechanisms, ?Mechanisms(Type)}).
mechanism({Mechanisms, [H|T]}) -> case lists:member(H, Mechanisms) of
	true -> {ok, H}; false -> mechanism({Mechanisms, T}) end;
mechanism({_Mechanisms, []}) -> {error, no_supported_mechanism}.

jid(Features) -> case lists:keyfind(bind, #result.id, Features) of
	#result{data = #stanza{content = Jid}} -> {ok, Jid}; 
	false -> {error, not_found}
end.

access_token(#auth{oauth = #oauth2{token = #oauth2_token{access = T}}}) -> T.
client_id(#auth{oauth = #oauth2{client = #oauth2_client{id = ID}}}) -> ID.

user_name(#auth{user_name = UserName}) -> UserName.
password(#auth{password = Password}) -> Password.
