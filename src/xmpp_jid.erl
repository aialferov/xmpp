%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 11 Sep 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_jid).
-compile(export_all).

bare(Jid) -> bare(Jid, []).
bare("/" ++ _, Acc) -> lists:reverse(Acc);
bare([H|T], Acc) -> bare(T, [H|Acc]);
bare([], Acc) -> lists:reverse(Acc).

local_part(Jid) -> local_part(Jid, []).
local_part("@" ++ _, Acc) -> lists:reverse(Acc);
local_part([H|T], Acc) -> local_part(T, [H|Acc]);
local_part([], Acc) -> lists:reverse(Acc).

domain_part(Jid) -> domain_part(Jid, false).
domain_part("@" ++ T, _) -> domain_part(T, []);
domain_part("/" ++ _, false) -> [];
domain_part("/" ++ _, Acc) -> lists:reverse(Acc);
domain_part([_|T], false) -> domain_part(T, false);
domain_part([H|T], Acc) -> domain_part(T, [H|Acc]);
domain_part([], false) -> [];
domain_part([], Acc) -> lists:reverse(Acc).

resource_part("/" ++ T) -> T;
resource_part([_|T]) -> resource_part(T);
resource_part([]) -> [].
