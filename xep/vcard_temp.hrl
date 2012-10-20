%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 11 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

%% XEP-0054

-define(XepVCard(Id, From, To), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("from", From),
		?XmlAttr("to", To), ?XmlAttr("type", "get")], [],
	?XmlEl("vCard", [], "vcard-temp", [])
)).

-define(XepVCardIn(VCard), [#xmlElement{name = 'vCard', content = VCard,
	namespace = #xmlNamespace{default = 'vcard-temp'}}]).
