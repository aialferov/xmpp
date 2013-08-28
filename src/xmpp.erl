%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 23 Aug 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp).
-compile(export_all).

-include("oauth2.hrl").

-include("utils_sasl.hrl").
-include("utils_monad.hrl").

-include("../src/rfc/xmpp_core_tools.hrl").

-record(auth, {oauth, user_name, password}).

-define(HostName(Network), case Network of
	live -> "messenger.live.com";
	google -> "google.com";
	facebook -> "chat.facebook.com";
	vkontakte -> "vk.com";
	odnoklassniki -> "ok.ru"
end).

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

login(Network, OAuth) -> login({Network, #auth{oauth = OAuth}}).
login(Network, UserName, Password) -> login({Network,
	#auth{user_name = UserName, password = Password}}).

%negotiate(Network, Auth, Pid) -> negotiate(Network, Auth, Pid,
%	xmpp_gate:open_stream(?HostName(Network), Pid)).
%
%login({Network, Auth}) ->
%	{ok, Pid} = xmpp_gate:connect(?HostName(Network)),
%	ok = negotiate(Network, Auth, Pid),
%	{ok, Pid1} = negotiate(Network, Auth, Pid),
%	ok = negotiate(Network, Auth, Pid1),
%	{ok, sasl_success} = negotiate(Network, Auth, Pid1),
%	negotiate(Network, Auth, Pid1);

login({Network, Auth}) -> case xmpp_gate:connect(?HostName(Network)) of
	{ok, Pid} -> login({Network, Auth, Pid}); Error -> Error end;

login({Network, Auth, Pid}) -> login({Network, Auth, Pid, negotiate(
	Network, Auth, Pid, xmpp_gate:open_stream(?HostName(Network), Pid))});

login({Network, Auth, Pid, Result}) -> case Result of
	ok -> login({Network, Auth, Pid});
	{ok, NewPid} when is_pid(NewPid) -> login({Network, Auth, NewPid});
	{ok, sasl_success} -> login({Network, Auth, Pid});
	{ok, {sasl_failure, _, _}} -> {error, sasl_failure};
	{ok, Features} -> {ok, Jid} = jid(Features), {ok, {Jid, Pid}};
	Error -> Error
end.

negotiate(Network, Auth, Pid, {ok, Result}) -> negotiate(utils_monad:do(
	negotiation_funs(Network, Auth, Pid, Result)));
negotiate(_Network, _Auth, _Pid, Error) -> Error.

negotiate([#result{data = undefined}|_]) -> ok;
negotiate([#result{data = Result}|_]) -> {ok, Result};
negotiate([#error{reason = Reason}|_]) -> {error, Reason}.

negotiate_features(Network, Auth, Pid, Features) -> negotiate_features(
	utils_monad:do([feature_fun(Network, Auth, Pid, F) || F <- Features])).

negotiate_features([]) -> ok;
negotiate_features(Results = [#result{}|_]) -> {ok, Results};
negotiate_features(Results = [#error{}|_]) -> {error, Results}.

feature_fun(Network, _Auth, Pid, Feature) -> {Name, Args} = case Feature of
	bind -> {fun xmpp_gate:bind/1, [Pid]};
	session -> {fun xmpp_gate:establish_session/2, [?HostName(Network), Pid]};
	Feature -> {fun() -> {ok, not_supported} end, []}
end, #function{id = Feature, name = Name, args = Args}.

negotiation_funs(_Network, _Auth, _Pid,
	?XmppStreamError('see-other-host', Host))
-> [
	#function{id = h, name = fun() ->
		{ok, xmpp_gate:read_host(Host)} end, args = []},
	#function{id = c, name = fun({HostName, Port}) ->
		xmpp_gate:connect(HostName, Port) end, args = [#placeholder{id = h}]}
];

negotiation_funs(_Network, _Auth, _Pid, ?XmppStreamError(Name, Value)) -> [
	#function{id = e, name = fun() -> {error, {Name, Value}} end, args = []}
];

negotiation_funs(_Network, _Auth, Pid, {features, [Tls|_]})
	when Tls == start_tls; Tls == {start_tls, required}
-> [
	#function{id = s, name = fun xmpp_gate:start_tls/1, args = [Pid]},
	#function{id = c, name = fun xmpp_gate:connect_tls/1, args = [Pid]}
];

negotiation_funs(Network, Auth, Pid,
	{features, [{mechanisms, Mechanisms}|_]})
-> [
	#function{id = n, name = fun negotiate/4, args = [
		Network, Auth, Pid, mechanism(Mechanisms)]}
];

negotiation_funs(Network, Auth, Pid, {features, Features}) -> [
	#function{id = f, name = fun negotiate_features/4,
		args = [Network, Auth, Pid, Features]}
];

negotiation_funs(_Network, Auth, Pid, Mechanism = "OAUTH") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3, args = [
		Mechanism, utils_sasl:plain_message("=", access_token(Auth)), Pid]}
%		Mechanism, access_token(Auth), Pid]}
];

negotiation_funs(_Network, Auth, Pid, Mechanism = "X-OAUTH2") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3, args = [
		Mechanism, utils_sasl:plain_message("=", access_token(Auth)), Pid]}
];

negotiation_funs(_Network, Auth, Pid, Mechanism = "X-MESSENGER-OAUTH2") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3, args = [
		Mechanism, access_token(Auth), Pid]}
];

negotiation_funs(_Network, Auth, Pid, Mechanism = "X-FACEBOOK-PLATFORM") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, "=", Pid]},
	#function{id = r1, name = fun({sasl_challenge, Challenge}) ->
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
		Pid)
	end, args = [#placeholder{id = b}]}
];

negotiation_funs(_Network, Auth, Pid, Mechanism = "PLAIN") -> [
	#function{id = begin_sasl, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, utils_sasl:plain_message(
			user_name(Auth), password(Auth)), Pid]
	}
];

negotiation_funs(Network, Auth, Pid, Mechanism = "DIGEST-MD5") -> [
	#function{id = b, name = fun xmpp_gate:begin_sasl/3,
		args = [Mechanism, "=", Pid]},
	#function{id = r1, name = fun({sasl_challenge, Challenge}) ->
		xmpp_gate:send_sasl_response(utils_sasl:digest_md5_response(
			user_name(Auth), password(Auth), ?HostName(Network),
			Challenge, ?DigestMd5Config
		), Pid)
	end, args = [#placeholder{id = b}]},
	#function{id = r2, name = fun xmpp_gate:send_sasl_response/2,
		args = ["", Pid]}
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
