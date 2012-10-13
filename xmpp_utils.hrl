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
