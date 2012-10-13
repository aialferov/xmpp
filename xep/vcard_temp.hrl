%% XEP-0054

-define(XepVCard(Id, From, To), ?XmlEl("iq",
	[?XmlAttr("id", Id), ?XmlAttr("from", From),
		?XmlAttr("to", To), ?XmlAttr("type", "get")], [],
	?XmlEl("vCard", [], "vcard-temp", [])
)).

-define(XepVCardIn(VCard), [#xmlElement{name = 'vCard', content = VCard,
	namespace = #xmlNamespace{default = 'vcard-temp'}}]).
