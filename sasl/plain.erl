-module(plain).
-export([message/2])

message(AuthcID, Passwd) ->
	binary_to_list(base64:encode([0] ++ AuthcID ++ [0] ++ Passwd)).
