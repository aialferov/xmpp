%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 23 Aug 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp).

-export([login/2, login/3]).
-export([logout/1]).

-export([send_presence/3]).
-export([send_message/4]).

-export([read/2]).

-include("xmpp.hrl").
-include("utils_monad.hrl").

login(Network, OAuth) -> login({Network, xmpp_auth:make_auth(OAuth)}).
login(Network, UserName, Password) ->
	login({Network, xmpp_auth:make_auth(UserName, Password)}).

login({Network, Auth}) -> result(utils_monad:do([
	?Function(connect, fun xmpp_request:connect/1, [?HostName(Network)]),
	?Function(negotiate, fun xmpp_auth:negotiate/3,
		[Network, Auth, ?Placeholder(connect)]),
	?Function(tcp, fun tcp/1, [?Placeholder(negotiate)]),
	?Function(features, fun features/1, [?Placeholder(negotiate)]),
	?Function(jid, fun xmpp_auth:jid/1, [?Placeholder(features)]),
	?Function(roster, fun xmpp_request:roster_get/2,
		[?Placeholder(jid), ?Placeholder(tcp)]),
	?Function(presence, fun xmpp_request:send_presence/1, [?Placeholder(tcp)]),
	?Function(active_once, fun xmpp_transport:set_active/2,
		[?Placeholder(tcp), once]),
	?Function(result, fun(Jid, Roster, Tcp) -> {ok, {Jid, Roster, Tcp}} end,
		[?Placeholder(jid), ?Placeholder(roster), ?Placeholder(tcp)])
])).

logout(Tcp) ->
	xmpp_transport:set_active(Tcp, false),
	xmpp_request:close_stream(Tcp).

send_presence(ToJid, Type, Tcp) ->
	xmpp_request:send_presence(ToJid, Type, Tcp).

send_message(FromJid, ToJid, Body, Tcp) ->
	xmpp_request:send_message(FromJid, ToJid, Body, Tcp).

read({push, Stanza}, _Tcp) -> {push, Stanza};
read(Message, Tcp) -> result(utils_monad:do([
	?Function(dispatch, fun xmpp_transport:tcp_dispatch/2, [Message, Tcp]),
	?Function(log, fun io:format/2, ["Read: ~p~n", [?Placeholder(dispatch)]]),
	?Function(active_false, fun xmpp_transport:set_active/2, [Tcp, false]),
	?Function(read, fun xmpp_reader:read_data/2,
		[?Placeholder(dispatch), {xmpp_transport, wait, [Tcp]}]),
	?Function(active_once, fun xmpp_transport:set_active/2, [Tcp, once]),
	?Function(result, fun(Result) -> {ok, Result} end, [?Placeholder(read)])
])).

result(?Result(Data)) -> {ok, Data};
result(?Error(Reason)) -> {error, Reason}.

tcp({Tcp, _Features}) -> {ok, Tcp}.
features({_Tcp, Features}) -> {ok, Features}.
