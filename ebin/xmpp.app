%%%-------------------------------------------------------------------
%%% Created: 24 May 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, xmpp, [
	{id, "xmpp"},
	{vsn, "0.0.1"},
	{description, "XMPP client"},
	{modules, [
		xmpp,
		xmpp_jid,
		xmpp_auth,
		xmpp_reader,
		xmpp_result,
		xmpp_request,
		xmpp_transport
	]},
	{registered, []},
	{applications, [kernel, stdlib, ssl, inets, utils, oauth2]}
]}.
