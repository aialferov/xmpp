%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 21 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

%% RFC 6120

%% Outgoing packets

-define(XmppNsTls, "urn:ietf:params:xml:ns:xmpp-tls").
-define(XmppNsSasl, "urn:ietf:params:xml:ns:xmpp-sasl").
-define(XmppNsBind, "urn:ietf:params:xml:ns:xmpp-bind").

-define(XmppStreamHeader(From, To),
	"<?xml version='1.0'?>" ++
	"<stream:stream" ++
		?XmlAttr("from", From) ++
		" to='" ++ To ++ "'" ++
		" version='1.0'" ++
		" xml:lang='en'" ++
		" xmlns='jabber:client'" ++
		" xmlns:stream='http://etherx.jabber.org/streams'>"
).
-define(XmppStreamClosingTag, "</stream:stream>").

-define(XmppStartTlsCommand, ?XmlEl("starttls", [], ?XmppNsTls, [])).

-define(XmppSaslBeginCommand(Mechanism, InitialResponse), ?XmlEl("auth",
	[?XmlAttr("mechanism", Mechanism)], ?XmppNsSasl, InitialResponse)).
-define(XmppSaslResponse(Response),
	?XmlEl("response", [], ?XmppNsSasl, Response)).
-define(XmppSaslAbortCommand, ?XmlEl("abort", [], ?XmppNsSasl, [])).

-define(XmppBind(Id, Resource), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("type", "set")], [],
	?XmlEl("bind", [], ?XmppNsBind, ?XmlElSimple("resourse", Resource))
)).

-define(XmppStanzaResult(Id, From), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("from", From), ?XmlAttr("type", "result")],
	[], []
)).

%% Incoming packets

-define(XmppStreamTagName, 'stream:stream').
-define(XmppStreamFeaturesTagName, 'stream:features').
-define(XmppStreamErrorTagName, 'stream:error').

-define(XmppStreamClosingTagName, "/stream:stream").

-define(XmppStreamHeaderIn(Content),
	#xmlElement{name = ?XmppStreamTagName, content = Content}).

-define(XmppStreamFeaturesIn(Content),
	#xmlElement{name = ?XmppStreamFeaturesTagName, content = Content}).
-define(XmppStreamFeatureStartTlsIn(Content),
	#xmlElement{name = 'starttls', content = Content}).
-define(XmppStreamFeatureMechanismsIn(Content),
	#xmlElement{name = 'mechanisms', content = Content}).
-define(XmppStreamFeatureBindIn(Content),
	#xmlElement{name = 'bind', content = Content}).
-define(XmppStreamFeatureSubIn(Content),
	#xmlElement{name = 'sub', content = Content}).

-define(XmppStartTlsProceedIn, #xmlElement{name = proceed,
	namespace = #xmlNamespace{default = 'urn:ietf:params:xml:ns:xmpp-tls'}}).
-define(XmppStartTlsFailureIn, #xmlElement{name = failure,
	namespace = #xmlNamespace{default = 'urn:ietf:params:xml:ns:xmpp-tls'}}).

-define(XmppStreamErrorIn(Condition, Value, Optional),
	#xmlElement{name = ?XmppStreamErrorTagName, content = [
		#xmlElement{name = Condition, content = Value}|Optional]}
). 

-define(XmppSaslSuccessIn, #xmlElement{name = success,
	namespace = #xmlNamespace{default = 'urn:ietf:params:xml:ns:xmpp-sasl'}}).

-define(XmppSaslFailueIn(Condition, Text), #xmlElement{name = failure,
	content = [#xmlElement{name = Condition}|Text],
	namespace = #xmlNamespace{default = 'urn:ietf:params:xml:ns:xmpp-sasl'}
}).
-define(XmppSaslChallengeIn(Content), #xmlElement{
	name = challenge, content = [#xmlText{value = Content}],
	namespace = #xmlNamespace{default = 'urn:ietf:params:xml:ns:xmpp-sasl'}
}).

-define(XmppStanzaIn(Stanza, Attributes, Content),
	#xmlElement{name = Stanza, attributes = Attributes, content = Content}
		when Stanza == iq orelse Stanza == presence orelse Stanza == message
).

-define(XmppStanzaErrorIn(Attributes, Condition, Value, Optional),
	#xmlElement{name = error, attributes = Attributes,
		content = [#xmlElement{name = Condition, content = Value}|Optional]}
).
-define(XmppStanzaErrorTextIn(Attributes, Text),
	#xmlElement{name = text, attributes = Attributes, content = Text}).

-define(XmppBindIn(Jid), [#xmlElement{name = bind,
	content = [#xmlElement{name = jid, content = Jid}]}]).
