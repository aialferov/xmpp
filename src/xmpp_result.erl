%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_result).
-export([read/3]).

-include("rfc/xmpp_core_tools.hrl").

read(StanzaId, Response, Tcp) -> read({StanzaId,
	read_response(StanzaId, Response), Tcp}).

read({StanzaId, {ok, Response}, Tcp}) -> case Response of
	whitespace ->
		io:format("~p~n", [Response]),
		xmpp_transport:response(StanzaId, Tcp);
	{push, _Stanza} ->
		io:format("~p~n", [Response]),
		xmpp_transport:response(StanzaId, Tcp);
	{back, Stanza} -> {ok, Stanza};
	StreamError = #streamError{} -> complete_stream_error(
		handle_stream_error(StreamError, Tcp), Tcp);
	Response -> {ok, Response}
end;
read({_StanzaId, Error, _Tcp}) -> Error.

read_response(StanzaId, {ok, Stanza = #stanza{
	attributes = #stanzaAttributes{id = StanzaId}}}) -> {ok, {back, Stanza}};
read_response(_StanzaId, {ok, Stanza = #stanza{}}) -> {ok, {push, Stanza}};
read_response(_StanzaId, Response) -> Response.

complete_stream_error(StreamError, Tcp) -> complete_stream_error(
	StreamError, Tcp, xmpp_transport:response(false, Tcp)).

complete_stream_error(StreamError, Tcp, Response) -> case Response of
	{ok, stream_closed} -> complete_stream_error(StreamError, Tcp);
	{error, closed} -> {ok, StreamError};
	Error -> Error
end.

handle_stream_error(Se = #streamError{condition =
	C = #condition{name = 'see-other-host', value = {HostName, Port}}}, Tcp)
-> 
	Se#streamError{condition = C#condition{value = {HostName,
		case Port of 0 -> xmpp_transport:port(Tcp); Port -> Port end}}}.
