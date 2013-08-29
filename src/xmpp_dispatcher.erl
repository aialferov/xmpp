%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_dispatcher).

-export([init/1, loop/1]).
-export([enqueue/2, enqueue/3]).

-define(Timeout, 30000).

init(Tcp) -> case gen_tcp:controlling_process(xmpp_transport:socket(Tcp),
	Pid = spawn_link(?MODULE, loop, [Tcp]))
of ok -> {ok, Pid}; Error -> Error end.

enqueue(Command, Pid) -> enqueue(Command, [], Pid).
enqueue(Command, Args, Pid) ->
	enqueue(Command, Args, Pid, is_process_alive(Pid)).
enqueue(Command, Args, Pid, true) ->
	io:format("Command: ~p ~p~n", [self(), Command]),
	Pid ! case Args of [] -> {Command, self()};
		Args -> {Command, Args, self()} end,
	receive Reply -> Reply after ?Timeout -> {error, timeout} end;
enqueue(_Command, _Args, _Pid, false) -> {error, not_found}.

loop(Tcp) -> receive
	{connect_tls, From} -> case xmpp_transport:connect_tls(Tcp) of
		{ok, Tls} -> reply(From, ok, Tls); Error -> reply(From, Error) end;
	{close, From} ->
		reply(From, xmpp_transport:close(Tcp));
	{request, [StanzaId, Data], From} ->
		reply(From, xmpp_transport:request(StanzaId, Data, Tcp), Tcp);
	{request_async, [Data], From} ->
		reply(From, xmpp_transport:send(Data, Tcp), Tcp);
	Message -> tcp_dispatch(Message, Tcp)
end.

tcp_dispatch(Message, Tcp) -> case xmpp_transport:tcp_dispatch(Message, Tcp) of
	{ok, Data} ->
		io:format("Spontaneous data: ~p ~p~n", [self(), Data]),
%		io:format("Spontaneous data: ~p ~p~n",
%			[self(), xmpp_transport:read(false, Data, Tcp)]),
		loop(Tcp);
	{error, Reason} ->
		io:format("Spontaneous error: ~p ~p~n", [self(), Reason]),
		{error, Reason}
end.

reply(From, Reply) -> From ! Reply.
reply(From, ok, Tcp) -> From ! ok, loop(Tcp);
reply(From, {ok, Reply}, Tcp) -> From ! {ok, Reply}, loop(Tcp);
reply(From, Error, _Tcp) -> From ! Error.
