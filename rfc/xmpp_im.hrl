%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 04 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

%% RFC 6121

%% Outgoing packets

-define(XmppNsRoster, "jabber:iq:roster").

-define(XmppRosterGet(Id, From), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("from", From), ?XmlAttr("type", "get")], [],
	?XmlEl("query", [], ?XmppNsRoster, [])
)).
-define(XmppRosterSet(Id, From, Jid, Name, Subscription, Groups), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("from", From), ?XmlAttr("type", "set")], [],
	?XmlEl("query", [], ?XmppNsRoster, ?XmlEl("item",
		[?XmlAttr("jid", Jid), ?XmlAttr("name", Name),
			?XmlAttr("subscription", Subscription)], [],
		[?XmlElSimple("group", Group) || Group <- Groups]
	))
)).

-define(XmppPresence(Id, To, Type), ?XmlEl("presence",
	[?XmlAttr("id", Id), ?XmlAttr("to", To), ?XmlAttr("type", Type)], [], []
)).

-define(XmppMessage(Id, From, To, Body), ?XmlEl("message",
	[?XmlAttr("id", Id), ?XmlAttr("from", From), ?XmlAttr("to", To),
		?XmlAttr("type", "chat"), ?XmlAttr("xml:lang", "en")], [],
	[?XmlElSimple("body", Body)]
)).

%% Incoming packets

-define(XmppRosterIn(Items), [#xmlElement{name = 'query', content = Items,
	namespace = #xmlNamespace{default = 'jabber:iq:roster'}}]).
-define(XmppRosterItemIn(Item), #xmlElement{name = item, attributes = Item}).
