-module(xmpp).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([connect/2, connect/3, connect_tls/1, close/1]).
-export([open_stream/3, close_stream/1, start_tls/1]).
-export([begin_sasl/3, send_sasl_response/2]).
-export([bind/2, establish_session/2]).
-export([roster_get/2, roster_set/3]).
-export([send_presence/3]).
-export([send_message/4]).
-export([send_stanza_result/3]).
-export([request_vcard/3]).
-export([send_raw_xml/2]).
-export([init/1, handle_call/3, handle_cast/2,
	handle_info/2, terminate/2, code_change/3]).

start_link() ->	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).

connect(ID, HostName) -> call({connect, ID, HostName}).
connect(ID, HostName, Port) -> call({connect, ID, HostName, Port}).
connect_tls(ID) -> call({connect_tls, ID}).
close(ID) -> call({close, ID}).

open_stream(ID, FromJid, Service) -> call({open_stream, ID, FromJid, Service}).
close_stream(ID) -> call({close_stream, ID}).

start_tls(ID) -> call({start_tls, ID}).
begin_sasl(ID, Mechanism, InitialResponse) ->
	call({begin_sasl, ID, Mechanism, InitialResponse}).
send_sasl_response(ID, Response) -> call({send_sasl_response, ID, Response}).
bind(ID, Resource) -> call({bind, ID, Resource}).
establish_session(ID, ToJid) -> call({establish_session, ID, ToJid}).

roster_get(ID, FromJid) -> call({roster_get, ID, FromJid}).
roster_set(ID, FromJid, Item) -> call({roster_set, ID, FromJid, Item}).

send_presence(ID, ToJid, Type) -> call({send_presence, ID, ToJid, Type}).
send_message(ID, FromJid, ToJid, Body) ->
	call({send_message, ID, FromJid, ToJid, Body}).

send_stanza_result(ID, StanzaId, FromJid) -> 
	call({send_stanza_result, ID, StanzaId, FromJid}).

request_vcard(ID, FromJid, ToJid) -> call({request_vcard, ID, FromJid, ToJid}).

send_raw_xml(ID, Xml) -> call({send_raw_xml, ID, Xml}).

call(Message) -> gen_server:call(?MODULE, Message, infinity).


init(_Args) -> process_flag(trap_exit, true), {ok, []}.

handle_connect(ID, Arg, From, State) ->
	case lists:keyfind(ID, 1, State) of
		false ->
			Pid = spawn_link(xmpp_gate, connect, [Arg, From]),
			{noreply, [{ID, Pid, From}|State]};
		_Found -> {reply, {error, already_connected}, State}
	end.
handle_request(HandleRequest, ID, From, State) ->
	case lists:keyfind(ID, 1, State) of
		false -> {reply, {error, not_connected}, State};
		{ID, Pid, _OldFrom} ->
			case HandleRequest(Pid, From) of
				error -> {reply, {error, not_connected}, State};
				ok -> {noreply, lists:keyreplace(
					ID, 1, State, {ID, Pid, From})}
			end
	end.

f(F) -> fun(Pid, From) -> xmpp_gate:F(Pid, From) end.
f(F, A1) -> fun(Pid, From) -> xmpp_gate:F(A1, Pid, From) end.
f(F, A1, A2) -> fun(Pid, From) -> xmpp_gate:F(A1, A2, Pid, From) end.
f(F, A1, A2, A3) -> fun(Pid, From) -> xmpp_gate:F(A1, A2, A3, Pid, From) end.

handle_call({connect, ID, HostName}, From, State) ->
	handle_connect(ID, HostName, From, State);
handle_call({connect, ID, HostName, Port}, From, State) ->
	handle_connect(ID, {HostName, Port}, From, State);

handle_call({Request, ID}, From, State) ->
	handle_request(f(Request), ID, From, State);
handle_call({Request, ID, A1}, From, State) ->
	handle_request(f(Request, A1), ID, From, State);
handle_call({Request, ID, A1, A2}, From, State) ->
	handle_request(f(Request, A1, A2), ID, From, State);
handle_call({Request, ID, A1, A2, A3}, From, State) ->
	handle_request(f(Request, A1, A2, A3), ID, From, State).

handle_cast(stop, State) -> {stop, normal, State}.
handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
	{noreply, lists:delete(case Info of
		{'EXIT', Pid, normal} -> lists:keyfind(Pid, 2, State);
        {'EXIT', Pid, Reason} ->
			{ID, Pid, From} = lists:keyfind(Pid, 2, State),
			gen_server:reply(From, {error, Reason}),
			{ID, Pid, From}
	end, State)}.

terminate(Reason, _State) -> io:format("Terminate: ~p~n", [Reason]), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

