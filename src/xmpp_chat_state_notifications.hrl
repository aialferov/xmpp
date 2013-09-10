%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 10 Sep 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

%% XEP-0085

-define(XepChatStateNotification(Type), #xmlElement{name = Type, namespace =
	#xmlNamespace{default = 'http://jabber.org/protocol/chatstates'}}).
