%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-lp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 23 Aug 2013 by Anton I Alferov <casper@ubca-lp>
%%%-------------------------------------------------------------------

-define(HostName(Network), case Network of
	live -> "messenger.live.com";
	google -> "google.com";
	facebook -> "chat.facebook.com";
	vkontakte -> "vk.com";
	odnoklassniki -> "ok.ru"
end).
