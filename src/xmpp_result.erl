%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_result).
-export([read/3]).

-include("rfc/xmpp_core_tools.hrl").

read(StanzaId, Response, Tcp) -> case read_response(StanzaId, Response) of
	{ok, whitespace} -> xmpp_transport:response(StanzaId, Tcp);
	{ok, {push, Stanza}} ->
		xmpp_transport:push_stanza(Stanza),
		xmpp_transport:response(StanzaId, Tcp);
	{ok, {back, Stanza}} -> {ok, Stanza};
	{ok, StreamError = #streamError{}} -> complete_stream_error(
		handle_stream_error(StreamError, Tcp), Tcp);
	Other -> Other
end.

read_response(StanzaId, {ok, Stanza = #stanza{
	attributes = #stanzaAttributes{id = StanzaId}}}) -> {ok, {back, Stanza}};
read_response(_StanzaId, {ok, Stanza = #stanza{}}) -> {ok, {push, Stanza}};
read_response(_StanzaId, Response) -> Response.

complete_stream_error(StreamError, Tcp) ->
	case xmpp_transport:response(false, Tcp) of
		{ok, stream_closed} -> complete_stream_error(StreamError, Tcp);
		{error, closed} -> {ok, StreamError};
		Error -> Error
	end.

handle_stream_error(Se = #streamError{condition =
	C = #condition{name = 'see-other-host', value = {HostName, Port}}}, Tcp)
-> 
	Se#streamError{condition = C#condition{value = {HostName,
		case Port of 0 -> xmpp_transport:port(Tcp); Port -> Port end}}};
handle_stream_error(Error, _Tcp) -> Error.
