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

-include("xmpp_utils.hrl").

-include("rfc/xmpp_core.hrl").

-include("rfc/xmpp_im.hrl").
-include("rfc/xmpp_im_obsolete.hrl").

-include("xep/vcard_temp.hrl").

-define(StanzaIDFormat, [4]).

connect(HostName, Port) ->
	init_dispatcher(xmpp_transport:connect(HostName, Port)).

connect(HostName) -> init_dispatcher(xmpp_transport:connect(HostName)).

connect_tls(Pid) -> xmpp_dispatcher:enqueue(connect_tls, Pid).
close(Pid) -> xmpp_dispatcher:enqueue(close, Pid).

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

init_dispatcher({ok, Tcp}) -> xmpp_dispatcher:init(Tcp);
init_dispatcher(Error) -> Error.

send_request(Data, Pid) -> send_request(false, Data, Pid).
send_request(StanzaId, Data, Pid) ->
	xmpp_dispatcher:enqueue(request, [StanzaId, Data], Pid).

send_request_async(Data, Pid) ->
	xmpp_dispatcher:enqueue(request_async, [Data], Pid).

generate_stanza_id() -> utils_crypto:generate_nonce(?StanzaIDFormat).
