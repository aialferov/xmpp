-record(stanza, {stanza = "", attributes = "", content = ""}).
-record(stanzaAttributes, {id = "", from = "", to = "", type = "", lang = ""}).
-record(stanzaError, {by = "", type = "", condition = "",
	text = "", lang = "", application_condition = []}).
