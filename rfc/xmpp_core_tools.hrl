-record(condition, {name, value = ""}).
-record(optional, {text = "", app_condition = []}).

-record(streamError, {condition = #condition{}, optional = #optional{}}).

-record(stanza, {stanza, attributes = [], content = []}).
-record(stanzaAttributes, {id = "", from = "", to = "", type = "", lang = ""}).
-record(stanzaError, {by = "", type,
	condition = #condition{}, optional = #optional{}}).
