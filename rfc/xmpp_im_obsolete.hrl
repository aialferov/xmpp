%% RFC 3921

%% Outgoing packets

-define(XmppSession(Id, To), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("to", To), ?XmlAttr("type", "set")], [],
	?XmlEl("session", [], "urn:ietf:params:xml:ns:xmpp-session", [])
)).

%% Incoming packets

-define(XmppStreamFeatureSessionIn(Content),
	#xmlElement{name = 'session', content = Content}).

