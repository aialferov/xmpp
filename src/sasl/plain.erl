%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 13 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(plain).
-export([message/2]).

message(AuthcID, Passwd) ->
	binary_to_list(base64:encode([0] ++ AuthcID ++ [0] ++ Passwd)).
