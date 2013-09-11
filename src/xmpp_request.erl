%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_request).

-export([connect/2, connect/1, connect_tls/1, close/1]).
-export([open_stream/2, open_stream/3, close_stream/1]).

-export([start_tls/1]).
-export([begin_sasl/3, send_sasl_response/2]).
-export([bind/1, bind/2, establish_session/2]).

-export([roster_get/2, roster_set/3]).

-export([send_presence/1, send_presence/3]).
-export([send_message/4]).

-export([request_vcard/3]).

-export([send_stanza_result/4]).
-export([send_raw_xml/2]).

-include("xmpp_utils.hrl").

-include("xmpp_core.hrl").

-include("xmpp_im.hrl").
-include("xmpp_im_obsolete.hrl").

-include("xmpp_vcard_temp.hrl").

-define(StanzaIDFormat, [4]).

connect(HostName, Port) -> xmpp_transport:connect(HostName, Port).
connect(HostName) -> xmpp_transport:connect(HostName).

connect_tls(Tcp) -> xmpp_transport:connect_tls(Tcp).

close(Tcp) -> xmpp_transport:close(Tcp).

open_stream(ToJid, Tcp) -> open_stream([], ToJid, Tcp).
open_stream(FromJid, ToJid, Tcp) ->
	send_request(?XmppStreamHeader(FromJid, ToJid), Tcp).
close_stream(Tcp) -> send_request(?XmppStreamClosingTag, Tcp).

start_tls(Tcp) -> send_request(?XmppStartTlsCommand, Tcp).

begin_sasl(Mechanism, InitialResponse, Tcp) ->
	send_request(?XmppSaslBeginCommand(Mechanism, InitialResponse), Tcp).
send_sasl_response(Response, Tcp) ->
	send_request(?XmppSaslResponse(Response), Tcp).

bind(Tcp) -> bind([], Tcp).
bind(Resource, Tcp) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppBind(StanzaId, Resource), Tcp).

establish_session(ToJid, Tcp) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppSession(StanzaId, ToJid), Tcp).

roster_get(FromJid, Tcp) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppRosterGet(StanzaId, FromJid), Tcp).

roster_set(FromJid, {Jid, Name, Subscription, Groups}, Tcp) ->
	StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XmppRosterSet(StanzaId, FromJid,
		Jid, Name, Subscription, Groups), Tcp).

send_presence(Tcp) -> send_request_async(?XmppPresence([], [], []), Tcp).
send_presence(ToJid, Type, Tcp) -> StanzaId = generate_stanza_id(),
	send_request_async(?XmppPresence(StanzaId, ToJid, Type), Tcp).

send_message(FromJid, ToJid, Body, Tcp) -> StanzaId = generate_stanza_id(),
	send_request_async(?XmppMessage(StanzaId, FromJid, ToJid, Body), Tcp).

request_vcard(FromJid, ToJid, Tcp) -> StanzaId = generate_stanza_id(),
	send_request(StanzaId, ?XepVCard(StanzaId, FromJid, ToJid), Tcp).

send_stanza_result(StanzaId, FromJid, ToJid, Tcp) ->
	send_request_async(?XmppStanzaResult(StanzaId, FromJid, ToJid), Tcp).

send_raw_xml(Xml, Tcp) -> send_request_async(Xml, Tcp).

send_request(Data, Tcp) -> send_request(false, Data, Tcp).
send_request(StanzaId, Data, Tcp) ->
	xmpp_transport:request(StanzaId, Data, Tcp).

send_request_async(Data, Tcp) -> xmpp_transport:request_async(Data, Tcp).

generate_stanza_id() -> utils_crypto:generate_nonce(?StanzaIDFormat).
