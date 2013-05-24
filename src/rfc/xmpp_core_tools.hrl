%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 11 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-record(condition, {name, value = ""}).
-record(optional, {text = "", app_condition = []}).

-record(streamError, {condition = #condition{}, optional = #optional{}}).

-record(stanza, {stanza, attributes = [], content = []}).
-record(stanzaAttributes, {id = "", from = "", to = "", type = "", lang = ""}).
-record(stanzaError, {by = "", type,
	condition = #condition{}, optional = #optional{}}).
