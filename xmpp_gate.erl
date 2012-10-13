-module(xmpp_gate).

-export([connect/2, connect_tls/2, close/2]).
-export([open_stream/4, close_stream/2]).
-export([start_tls/2, begin_sasl/4, send_sasl_response/3,
	bind/3, establish_session/3]).
-export([roster_get/3, roster_set/4]).
-export([send_presence/4]).
-export([send_message/5]).
-export([send_stanza_result/4]).
-export([request_vcard/4]).

-include_lib("kernel/include/inet.hrl").

-include("xmpp_utils.hrl").

-include("rfc/xmpp_core.hrl").
-include("rfc/xmpp_core_tools.hrl").

-include("rfc/xmpp_im.hrl").
-include("rfc/xmpp_im_obsolete.hrl").

-include("xep/vcard_temp.hrl").

-define(SrvRrService, "xmpp-client").
-define(SrvRrProto, "tcp").
-define(SrvRr, "_" ++ ?SrvRrService ++ "._" ++ ?SrvRrProto ++ ".").

connect({HostName, Port}, From) ->
	{ok, Socket} = gen_tcp:connect(HostName, Port, []),
	gen_server:reply(From, ok),
	wait_command({Socket, gen_tcp, tcp, tcp_closed, tcp_error});

connect(HostName, From) ->
	{ok, #hostent{h_addr_list = SrvRrList}} =
		inet_res:getbyname(?SrvRr ++ HostName, srv),
	{ok, Socket} = connect_srv_rr(SrvRrList, []),
	gen_server:reply(From, ok),
	wait_command({Socket, gen_tcp, tcp, tcp_closed, tcp_error}).

connect_srv_rr([{_Priority, _Weight, Port, Target}|T], Reasons) ->
	case gen_tcp:connect(Target, Port, []) of
		{ok, Socket} -> {ok, Socket};
		{error, Reason} -> connect_srv_rr(T, [{Target, Port, Reason}|Reasons])
	end;
connect_srv_rr([], Reasons) -> {error, Reasons}.

connect_tls(Pid, From) -> send_command(Pid, {connect_tls, From}).
close(Pid, From) -> send_command(Pid, {close, From}).

open_stream(FromJid, ToJid, Pid, From) ->
	send_command(Pid, {open_stream, FromJid, ToJid, From}).
close_stream(Pid, From) -> send_command(Pid, {close_stream, From}).

start_tls(Pid, From) -> send_command(Pid, {start_tls, From}).
begin_sasl(Mechanism, InitialResponse, Pid, From) ->
	send_command(Pid, {begin_sasl, Mechanism, InitialResponse, From}).
send_sasl_response(Response, Pid, From) ->
	send_command(Pid, {send_sasl_response, Response, From}).
bind(Resource, Pid, From) -> send_command(Pid, {bind, Resource, From}).
establish_session(ToJid, Pid, From) ->
	send_command(Pid, {establish_session, ToJid, From}).

roster_get(FromJid, Pid, From) ->
	send_command(Pid, {roster_get, FromJid, From}).
roster_set(FromJid, Item, Pid, From) ->
	send_command(Pid, {roster_set, FromJid, Item, From}).

send_presence(ToJid, Type, Pid, From) ->
	send_command(Pid, {send_presence, ToJid, Type, From}).
send_message(FromJid, ToJid, Body, Pid, From) ->
	send_command(Pid, {send_message, FromJid, ToJid, Body, From}).

send_stanza_result(StanzaId, FromJid, Pid, From) ->
	send_command(Pid, {send_stanza_result, StanzaId, FromJid, From}).

request_vcard(FromJid, ToJid, Pid, From) ->
	send_command(Pid, {request_vcard, FromJid, ToJid, From}).

send_command(Pid, Command) -> case is_process_alive(Pid) of
	true -> Pid ! Command, ok; false -> error end.

wait_command(TcpSet = {Socket, Module, Tcp, TcpClosed, TcpError}) ->
receive
	{connect_tls, From} ->
		{ok, TlsSocket} = ssl:connect(Socket, []),
		gen_server:reply(From, ok),
		wait_command({TlsSocket, ssl, ssl, ssl_closed, ssl_error});

	{close, From} ->
		ok = Module:close(Socket),
		gen_server:reply(From, ok);

	{open_stream, FromJid, ToJid, From} ->
		send(?XmppStreamHeader(FromJid, ToJid), TcpSet, From);

	{close_stream, From} ->
		send(?XmppStreamClosingTag, TcpSet, From);

	{start_tls, From} ->
		send(?XmppStartTlsCommand, TcpSet, From);

	{begin_sasl, Mechanism, InitialResponse, From} ->
		send(?XmppSaslBeginCommand(
			Mechanism, InitialResponse), TcpSet, From);

	{send_sasl_response, Response, From} ->
		send(?XmppSaslResponse(Response), TcpSet, From);

	{bind, Resource, From} ->
		StanzaId = generate_stanza_id(),
		send(StanzaId, ?XmppBind(StanzaId, Resource), TcpSet, From);

	{establish_session, ToJid, From} ->
		StanzaId = generate_stanza_id(),
		send(StanzaId, ?XmppSession(StanzaId, ToJid), TcpSet, From);

	{roster_get, FromJid, From} ->
		StanzaId = generate_stanza_id(),
		send(StanzaId, ?XmppRosterGet(StanzaId, FromJid), TcpSet, From);

	{roster_set, FromJid, {Jid, Name, Subscription, Groups}, From} ->
		StanzaId = generate_stanza_id(),
		send(StanzaId, ?XmppRosterSet(StanzaId, FromJid,
			Jid, Name, Subscription, Groups), TcpSet, From);

	{send_presence, [], Type, From} ->
		send_async(?XmppPresence([], [], Type), TcpSet, From);
	{send_presence, ToJid, Type, From} ->
		StanzaId = generate_stanza_id(),
		send_async(?XmppPresence(StanzaId, ToJid, Type), TcpSet, From);

	{send_message, FromJid, ToJid, Body, From} ->
		StanzaId = generate_stanza_id(),
		send_async(?XmppMessage(StanzaId, FromJid, ToJid, Body), TcpSet, From);

	{send_stanza_result, StanzaId, FromJid, From} ->
		send(StanzaId, ?XmppStanzaResult(StanzaId, FromJid), TcpSet, From);

	{request_vcard, FromJid, ToJid, From} ->
		StanzaId = generate_stanza_id(),
		send(StanzaId, ?XepVCard(StanzaId, FromJid, ToJid), TcpSet, From);

	{Tcp, Socket, Data} ->
		io:format("Other data: ~p~n", [Data]),
		io:format("Other read data: ~p~n", [xmpp_reader:read_data(
			Data, fun() -> wait_more_data(TcpSet) end)]),
		wait_command(TcpSet);

	{TcpClosed, Socket} ->
		io:format("Connection closed~n");

	{TcpError, Socket, Reason} ->
		io:format("Connection error: ~p~n", [Reason])
end.

send_async(Data, TcpSet = {Socket, Module, _, _, _}, From) ->
	io:format("Send: ~p~n", [Data]),
	ok = Module:send(Socket, Data),
	gen_server:reply(From, ok),
	wait_command(TcpSet).

send(Data, TcpSet, From) -> send(null, Data, TcpSet, From).
send(StanzaId, Data, TcpSet = {Socket, Module, _, _, _}, From) ->
	io:format("Send: ~p~n", [Data]),
	ok = Module:send(Socket, Data),
	wait_response(StanzaId, TcpSet, From).

wait_response(StanzaId, TcpSet = {
	Socket, _Module, Tcp, TcpClosed, TcpError}, From) ->
receive
	{Tcp, Socket, Data} ->
		io:format("Data: ~p~n", [Data]),
		process_response(StanzaId, xmpp_reader:read_data(
			Data, fun() -> wait_more_data(TcpSet) end), TcpSet, From);
	{TcpClosed, Socket} -> gen_server:reply(From, {error, TcpClosed});
	{TcpError, Socket, Reason} ->
		gen_server:reply(From, {error, {TcpError, Reason}})
end.

wait_more_data({Socket, _Module, Tcp, TcpClosed, TcpError}) -> receive
	{Tcp, Socket, Data} -> io:format("More data: ~p~n", [Data]), {ok, Data};
	{TcpClosed, Socket} -> {error, TcpClosed};
	{TcpError, Socket, Reason} -> {error, {TcpError, Reason}}
end.

process_response(StanzaId, {ok, Stanza = #stanza{
	attributes = #stanzaAttributes{id = StanzaId}}}, TcpSet, From
) ->
	gen_server:reply(From, {ok, Stanza}),
	wait_command(TcpSet);
process_response(StanzaId, {ok, Stanza = #stanza{}}, TcpSet, From) ->
	io:format("Push stanza: ~p~n", [Stanza]),
	wait_response(StanzaId, TcpSet, From);
process_response(StanzaId, {ok, whitespace}, TcpSet, From) ->
	io:format("Push whitespace~n", []),
	wait_response(StanzaId, TcpSet, From);
process_response(_, Response, TcpSet, From) ->
	gen_server:reply(From, Response),
	wait_command(TcpSet).

generate_stanza_id() -> hex(binary_to_list(crypto:rand_bytes(4))).

hex([H|T]) -> [hex_digit(H bsr 4), hex_digit(H band 16#f) | hex(T)];
hex([]) -> [].

hex_digit(Dec) when Dec < 10 -> $0 + Dec;
hex_digit(Dec) -> $a + Dec - 10.

