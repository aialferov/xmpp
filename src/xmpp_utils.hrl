%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 09 Oct 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-define(XmlAttr(Name, Value), case Value of [] -> [];
	X -> " " ++ Name ++ "='" ++ X ++ "'" end).
-define(XmlEl(Name, Attributes, Namespace, Content),
	"<" ++ Name ++ 
		case Namespace of [] -> []; X -> " xmlns='" ++ X ++ "'" end ++
		lists:flatten(Attributes) ++
		fun([]) -> "/>"; (X) -> ">" ++ X ++
	"</" ++ Name ++ ">" end(lists:flatten(Content))
).
-define(XmlElSimple(Name, Content), case Content of [] -> [];
	X -> "<" ++ Name ++ ">" ++ X ++ "</" ++ Name ++">" end).
