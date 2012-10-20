%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 11 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

%% RFC 3921

%% Outgoing packets

-define(XmppSession(Id, To), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("to", To), ?XmlAttr("type", "set")], [],
	?XmlEl("session", [], "urn:ietf:params:xml:ns:xmpp-session", [])
)).

%% Incoming packets

-define(XmppStreamFeatureSessionIn(Content),
	#xmlElement{name = 'session', content = Content}).
