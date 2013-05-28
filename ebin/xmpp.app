%%%-------------------------------------------------------------------
%%% Created: 24 May 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, xmpp, [
	{id, "xmpp"},
	{vsn, "0.0.1"},
	{description, "XMPP client"},
	{modules, [
		xmpp,
		xmpp_gate,
		xmpp_reader
	]},
	{registered, []},
	{applications, [kernel, stdlib, ssl, inets, utils]}
]}.
