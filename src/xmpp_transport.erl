%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_transport).

-export([connect/1, connect/2, connect_tls/1, close/1]).

-export([request/3, response/2]).
-export([send/2, wait/1, read/3]).

-export([tcp_dispatch/2]).

-export([socket/1]).
-export([port/1]).

-include_lib("kernel/include/inet.hrl").

-record(tcp, {socket, module, proto, closed, error, port}).

-define(SrvRrService, "xmpp-client").
-define(SrvRrProto, "tcp").
-define(SrvRr, "_" ++ ?SrvRrService ++ "._" ++ ?SrvRrProto ++ ".").

-define(ConnectOptions, []).

connect(HostName, Port) ->
	make_tcp(gen_tcp:connect(HostName, Port, ?ConnectOptions)).

connect(HostName) -> case inet_res:getbyname(?SrvRr ++ HostName, srv) of
	{ok, #hostent{h_addr_list = SrvRrList}} -> connect_srv_rr(SrvRrList, []);
	Error -> Error
end.

connect_srv_rr([{_Priority, _Weight, Port, Target}|T], Reasons) ->
	case gen_tcp:connect(Target, Port, ?ConnectOptions) of
		{ok, Socket} -> make_tcp({ok, Socket});
		{error, Reason} -> connect_srv_rr(T, [{Target, Port, Reason}|Reasons])
	end;
connect_srv_rr([], Reasons) -> {error, Reasons}.

connect_tls(#tcp{socket = Socket}) ->
	make_tls(ssl:connect(Socket, ?ConnectOptions)).

close(#tcp{module = Module, socket = Socket}) -> Module:close(Socket).

request(StanzaId, Data, Tcp) -> case send(Data, Tcp) of
	ok -> response(StanzaId, Tcp); Error -> Error end.

response(StanzaId, Tcp) -> case wait(Tcp) of
	{ok, Data} -> read(StanzaId, Data, Tcp); Error -> Error end.

send(Data, #tcp{module = Module, socket = Socket}) ->
	io:format("Send: ~p ~p~n", [self(), Data]),
	Module:send(Socket, Data).

wait(Tcp) -> receive Message -> case tcp_dispatch(Message, Tcp) of
	{ok, Data} -> io:format("Wait: ~p ~p~n", [self(), Data]), {ok, Data};
	Error -> Error
end end.

read(StanzaId, Data, Tcp) -> xmpp_result:read(StanzaId,
	xmpp_reader:read_data(Data, {?MODULE, wait, [Tcp]}), Tcp).

tcp_dispatch(Message, #tcp{
	socket = Socket, proto = Proto,
	closed = Closed, error = Error
}) -> case Message of
	{Proto, Socket, Data} -> {ok, Data};
	{Closed, Socket} -> {error, closed};
	{Error, Socket, Reason} -> {error, {Error, Reason}}
end.

socket(#tcp{socket = Socket}) -> Socket.
port(#tcp{port = Port}) -> Port.

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
