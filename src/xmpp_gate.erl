%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_gate).

-export([connect/2, connect/1, connect_tls/1, close/1]).
-export([open_stream/2, open_stream/3, close_stream/1]).

-export([start_tls/1]).
-export([begin_sasl/3, send_sasl_response/2]).
-export([bind/1, bind/2, establish_session/2]).

-export([roster_get/2, roster_set/3]).

-export([send_presence/1, send_presence/3]).
-export([send_message/4]).

-export([request_vcard/3]).

-export([send_stanza_result/3]).
-export([send_raw_xml/2]).

-export([dispatch/1]).
-export([wait/1]).

-record(tcp, {socket, module, proto, closed, error, port}).

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

-define(ConnectOptions, []).
-define(Timeout, 30000).

-define(StanzaIDFormat, [4]).

connect(HostName, Port) -> spawn_dispatch(make_tcp(
	gen_tcp:connect(HostName, Port, ?ConnectOptions))).

connect(HostName) -> case inet_res:getbyname(?SrvRr ++ HostName, srv) of
	{ok, #hostent{h_addr_list = SrvRrList}} -> connect_srv_rr(SrvRrList, []);
	Error -> Error
end.

connect_srv_rr([{_Priority, _Weight, Port, Target}|T], Reasons) ->
	case gen_tcp:connect(Target, Port, ?ConnectOptions) of
		{ok, Socket} -> spawn_dispatch(make_tcp({ok, Socket}));
		{error, Reason} -> connect_srv_rr(T, [{Target, Port, Reason}|Reasons])
	end;
connect_srv_rr([], Reasons) -> {error, Reasons}.

connect_tls(Pid) -> dispatch(connect_tls, Pid).
close(Pid) -> dispatch(close, Pid).

open_stream(ToJid, Pid) -> open_stream([], ToJid, Pid).
open_stream(FromJid, ToJid, Pid) ->
	send_request(?XmppStreamHeader(FromJid, ToJid), Pid).
close_stream(Pid) -> send_request(?XmppStreamClosingTag, Pid).

start_tls(Pid) -> send_request(?XmppStartTlsCommand, Pid).

begin_sasl(Mechanism, InitialResponse, Pid) ->
	send_request(?XmppSaslBeginCommand(Mechanism, InitialResponse), Pid).
send_sasl_response(Response, Pid) ->
	send_request(?XmppSaslResponse(Response), Pid).

bind(Pid) -> bind([], Pid).
bind(Resource, Pid) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppBind(StanzaId, Resource), Pid).

establish_session(ToJid, Pid) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppSession(StanzaId, ToJid), Pid).

roster_get(FromJid, Pid) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppRosterGet(StanzaId, FromJid), Pid).

roster_set(FromJid, {Jid, Name, Subscription, Groups}, Pid) ->
	StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppRosterSet(StanzaId, FromJid,
		Jid, Name, Subscription, Groups), Pid).

send_presence(Pid) -> send_request_async(?XmppPresence([], [], []), Pid).
send_presence(ToJid, Type, Pid) -> StanzaId = generate_stanza_id(),
	send_request_async(?XmppPresence(StanzaId, ToJid, Type), Pid).

send_message(FromJid, ToJid, Body, Pid) -> StanzaId = generate_stanza_id(),
	send_request_async(?XmppMessage(StanzaId, FromJid, ToJid, Body), Pid).

request_vcard(FromJid, ToJid, Pid) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XepVCard(StanzaId, FromJid, ToJid), Pid).

send_stanza_result(StanzaId, FromJid, Pid) ->
	send_request(StanzaId, ?XmppStanzaResult(StanzaId, FromJid), Pid).

send_raw_xml(Xml, Pid) -> send_request_async(Xml, Pid).

send_request(Data, Pid) -> send_request(false, Data, Pid).
send_request(StanzaId, Data, Pid) ->
	dispatch({request, {StanzaId, Data}}, Pid).

send_request_async(Data, Pid) -> dispatch({request_async, Data}, Pid).

dispatch(Request, Pid) -> case is_process_alive(Pid) of
	true ->
		io:format("Request: ~p ~p~n", [Pid, Request]),
		Pid ! {Request, self()},
		receive Response -> Response after ?Timeout -> {error, timeout} end;
	false -> {error, not_found}
end.

dispatch(Tcp = #tcp{
	module = Module, socket = Socket,
	proto = Proto, closed = Closed, error = Error
}) -> receive

	{connect_tls, From} -> tls_upgrade(From, Socket);
	{close, From} -> reply(From, Module:close(Socket));
	{{request, {StanzaId, Data}}, From} ->
		reply(From, request(StanzaId, Data, Tcp), Tcp);
	{{request_async, Data}, From} -> reply(From, send(Data, Tcp), Tcp);
	
	{Proto, Socket, Data} ->
		io:format("Spontaneous data: ~p ~p~n",
			[self(), read(false, Data, Tcp)]),
		dispatch(Tcp);
	{Closed, Socket} ->
		io:format("Spontaneous close ~p~n", [self()]),
		{error, Closed};
	{Error, Socket, Reason} ->
		io:format("Spontaneous error: ~p ~p - ~p~n", [self(), Error, Reason]),
		{error, {Error, Reason}};
	Other -> io:format("Spontaneous: ~p ~p~n", [self(), Other])
end.

tls_upgrade(From, Socket) ->
	case make_tls(ssl:connect(Socket, ?ConnectOptions)) of
		{ok, Tls} -> reply(From, ok, Tls); Error -> reply(From, Error) end.

reply(From, Response) -> From ! Response.
reply(From, ok, Tcp) -> From ! ok, dispatch(Tcp);
reply(From, OK = {ok, _Response}, Tcp) -> From ! OK, dispatch(Tcp);
reply(From, Error, _Tcp) -> From ! Error.

request(StanzaId, Data, Tcp) -> result(StanzaId, case send(Data, Tcp) of
	ok -> response(StanzaId, Tcp); Error -> Error end, Tcp).

response(StanzaId, Tcp) -> case wait(Tcp) of
	{ok, Data} -> read(StanzaId, Data, Tcp); Error -> Error end.

result(StanzaId, {ok, Result}, Tcp) -> case Result of
	whitespace ->
		io:format("~p~n", [Result]),
		result(StanzaId, response(StanzaId, Tcp), Tcp);
	{push, _Stanza} ->
		io:format("~p~n", [Result]),
		result(StanzaId, response(StanzaId, Tcp), Tcp);
	{back, Stanza} -> {ok, Stanza};
	StreamError = #streamError{} -> complete_stream_error(
		handle_stream_error(StreamError, Tcp), Tcp);
	Result -> {ok, Result}
end;
result(_StanzaId, Error, _Tcp) -> Error.

complete_stream_error(StreamError, Tcp) ->
	complete_stream_error(StreamError, Tcp, response(false, Tcp)).
complete_stream_error(StreamError, Tcp = #tcp{closed = Closed}, Response) ->
	case Response of
		{ok, stream_closed} -> complete_stream_error(StreamError, Tcp);
		{error, Closed} -> {ok, StreamError};
		Error -> Error
	end.

handle_stream_error(Se = #streamError{condition =
	C = #condition{name = 'see-other-host', value = Host}}, Tcp)
-> 
	Se#streamError{condition = C#condition{value = case Host of
		{HostName, 0} -> {HostName, Tcp#tcp.port}; Host -> Host end}}.

send(Data, #tcp{module = Module, socket = Socket}) ->
	io:format("Send: ~p ~p~n", [self(), Data]),
	Module:send(Socket, Data).

wait(#tcp{
	socket = Socket, proto = Proto, 
	closed = Closed, error = Error
}) -> receive
	{Proto, Socket, Data} ->
		io:format("Wait: ~p ~p~n", [self(), Data]), {ok, Data};
	{Closed, Socket} -> {error, Closed};
	{Error, Socket, Reason} -> {error, {Error, Reason}}
end.

read(StanzaId, Data, Tcp) -> process_response(StanzaId,
	xmpp_reader:read_data(Data, fun() -> wait(Tcp) end)).

process_response(StanzaId, {ok, Stanza = #stanza{
	attributes = #stanzaAttributes{id = StanzaId}}}) -> {ok, {back, Stanza}};
process_response(_StanzaId, {ok, Stanza = #stanza{}}) -> {ok, {push, Stanza}};
process_response(_StanzaId, Response) -> Response.

spawn_dispatch({ok, Tcp = #tcp{socket = Socket}}) ->
	case gen_tcp:controlling_process(Socket, Pid =
		spawn_dispatch(spawn_link(?MODULE, dispatch, [Tcp])))
	of ok -> {ok, Pid}; Error -> Error end;
spawn_dispatch(Error) -> Error.

make_tcp({ok, Socket}) -> {ok, #tcp{
	socket = Socket, module = gen_tcp, proto = tcp,
	closed = tcp_closed, error = tcp_error, port = port(inet, Socket)
}};
make_tcp(Error) -> Error.
make_tls({ok, Socket}) -> {ok, #tcp{
	socket = Socket, module = ssl, proto = ssl,
	closed = ssl_closed, error = ssl_error, port = port(ssl, Socket)
}};
make_tls(Error) -> Error.

port(Module, Socket) -> case Module:peername(Socket) of
	{ok, {_Address, Port}} -> Port; _Error -> 0 end.

generate_stanza_id() -> utils_crypto:generate_nonce(?StanzaIDFormat).
