%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 11 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-record(rosterItem, {jid = "", name = "", subscription = "none", ask = ""}).
-record(presence, {show = "", status = [], priority = 0}).
-record(message, {subject, body, extensions = []}).
