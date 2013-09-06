%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 23 Aug 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp).

-export([login/2, login/3]).
-export([logout/1]).

-export([read/2]).

-include("xmpp.hrl").

login(Network, OAuth) -> login({Network, xmpp_auth:make_auth(OAuth)}).
login(Network, UserName, Password) ->
	login({Network, xmpp_auth:make_auth(UserName, Password)}).

login({Network, Auth}) -> case xmpp_gate:connect(?HostName(Network)) of
	{ok, Tcp} -> complete_login(xmpp_auth:negotiate(Network, Auth, Tcp));
	Error -> Error
end.

logout(Tcp) ->
	xmpp_transport:set_active(Tcp, false),
	xmpp_gate:close_stream(Tcp).

read(Message, Tcp) -> case xmpp_transport:tcp_dispatch(Message, Tcp) of
	{ok, Data} ->
		io:format("Read: ~p~n", [Data]),
		xmpp_transport:set_active(Tcp, false),
		Result = xmpp_reader:read_data(Data, {xmpp_transport, wait, [Tcp]}),
		xmpp_transport:set_active(Tcp, once), Result;
	Error -> Error
end.

complete_login({ok, {Tcp, Features}}) ->
	{ok, Jid} = xmpp_auth:jid(Features),
	xmpp_gate:roster_get(Jid, Tcp),
	xmpp_gate:send_presence(Tcp),
	xmpp_transport:set_active(Tcp, once),
	{ok, {Jid, Tcp}}.
