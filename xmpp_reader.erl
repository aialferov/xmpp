-module(xmpp_reader).
-export([read_data/2]).

-include_lib("xmerl/include/xmerl.hrl").

-include("xmpp_utils.hrl").

-include("rfc/xmpp_core.hrl").
-include("rfc/xmpp_core_tools.hrl").

-include("rfc/xmpp_im.hrl").
-include("rfc/xmpp_im_tools.hrl").

-include("rfc/xmpp_im_obsolete.hrl").

-include("xep/vcard_temp.hrl").
-include("xep/vcard_temp_tools.hrl").

read_data(Data, MoreDataFun) ->
	case gather_tag(Data) of
		{ok, whitespace} -> {ok, whitespace};
		{ok, ?XmppStreamClosingTagName} -> {ok, stream_closed};
		{ok, ElementName} ->
			{Document, _Rest} = xmerl_scan:string(Data, [
				{continuation_fun, fun more_data/3,
					{MoreDataFun, list_to_atom(ElementName)}},
				{hook_fun, fun switch_hook_state/2}
			]),
			{ok, build_response(Document)};
		{error, incomplete} -> case MoreDataFun() of
			{ok, MoreData} -> read_data(Data ++ MoreData, MoreDataFun);
			Error -> Error
		end
	end.

more_data(Continue, Exception, GlobalState) ->
	case more_data(xmerl_scan:cont_state(GlobalState),
		xmerl_scan:hook_state(GlobalState))
	of
		{ok, done} -> Exception(GlobalState);
		{ok, MoreData} -> Continue(MoreData, GlobalState);
		{error, Error} -> throw(Error)
	end.

switch_hook_state(Entity, GlobalState) -> case Entity of
	#xmlElement{name = HookState} ->
		{Entity, xmerl_scan:hook_state(HookState, GlobalState)};
	#xmlText{} -> {Entity, GlobalState}
end.

more_data({_, ?XmppStreamTagName}, ClosingTag)
	when ClosingTag == ?XmppStreamFeaturesTagName
	orelse ClosingTag == ?XmppStreamErrorTagName ->
		{ok, ?XmppStreamClosingTag};
more_data({_, StartingTag}, ClosingTag)
	when StartingTag == ClosingTag -> {ok, done};
more_data({MoreDataFun, _}, _) -> MoreDataFun().

gather_tag(" ") -> {ok, whitespace};
gather_tag("<" ++ T) -> gather_tag(T, []);
gather_tag([_|T]) -> gather_tag(T);
gather_tag([]) -> {error, incomplete}.

gather_tag(" " ++ T, "lmx?") -> gather_tag(T);
gather_tag(" " ++ T, []) -> gather_tag(T, []);
gather_tag("/" ++ T, []) -> gather_tag(T, "/");
gather_tag(" " ++ _, Element) -> {ok, lists:reverse(Element)};
gather_tag(">" ++ _, Element) -> {ok, lists:reverse(Element)};
gather_tag("/" ++ _, Element) -> {ok, lists:reverse(Element)};
gather_tag([H|T], Element) -> gather_tag(T, [H|Element]);
gather_tag([], _) -> {error, incomplete}.

build_response(Document) -> case Document of
	?XmppStreamHeaderIn([?XmppStreamFeaturesIn(Content)]) ->
		{features, read_features(Content)};

	?XmppStartTlsProceedIn -> tls_proceed;
	?XmppStartTlsFailureIn -> tls_failure;

	?XmppStreamHeaderIn([?XmppStreamErrorIn(
		'see-other-host', [#xmlText{value = Host}], Optional)]) ->
			{stream_error, {'see-other-host', Host}, read_optional(Optional)};

	?XmppStreamHeaderIn([?XmppStreamErrorIn(Condition, [], Optional)]) ->
		{stream_error, Condition, read_optional(Optional)};

	?XmppStreamErrorIn(Condition, [], Optional) ->
		{stream_error, Condition, read_optional(Optional)};

	?XmppSaslChallengeIn(Content) -> {sasl_challenge, Content};

	?XmppSaslSuccessIn -> sasl_success;

	?XmppSaslFailueIn(Condition, Optional) ->
		{sasl_failure, Condition, read_optional(Optional)};

	?XmppStanzaIn(Stanza, Attributes, Content) ->
		read_stanza(Stanza, Attributes, Content);

	Other -> Other
end.

read_features(Content) -> lists:map(fun
	(?XmppStreamFeatureStartTlsIn([])) -> start_tls;
	(?XmppStreamFeatureStartTlsIn([#xmlElement{name = required}])) ->
		{start_tls, required};

	(?XmppStreamFeatureMechanismsIn(Mechanisms)) ->
		{mechanisms, lists:map(fun
			(#xmlElement{name = mechanism,
				content = [#xmlText{value = Mechanism}]}) -> Mechanism
		end, Mechanisms)};

	(?XmppStreamFeatureBindIn([])) -> bind;
	(?XmppStreamFeatureBindIn([#xmlElement{name = required}])) ->
		{bind, required};

	(?XmppStreamFeatureSubIn([])) -> sub;

	(?XmppStreamFeatureSessionIn([])) -> session;
	(?XmppStreamFeatureSessionIn([#xmlElement{name = required}])) ->
		{session, required};

	(Other) -> Other
end, Content).

read_stanza(Stanza, Attributes, Content) ->
	StanzaAttributes = read_stanza_attributes(Attributes),
	#stanza{stanza = Stanza, attributes = StanzaAttributes,
		content = case StanzaAttributes of
			#stanzaAttributes{type = "error"} -> read_stanza_error(Content);
			_ -> read_stanza(Stanza, Content)
		end
	}.

read_stanza(iq, ?XmppBindIn(Jid)) -> read_text(Jid);
read_stanza(iq, ?XmppRosterIn(Items)) -> read_roster_items(Items);
read_stanza(iq, ?XepVCardIn(VCard)) -> read_vcard(VCard);
read_stanza(presence, Presence) -> read_presence(Presence);
read_stanza(message, Message) -> read_message(Message);
read_stanza(_, _) -> [].

read_roster_items(Items) -> lists:map(fun(?XmppRosterItemIn(Item)) -> 
	lists:foldl(fun
		(#xmlAttribute{name = Name, value = Value}, OldItem) -> case Name of
			ask -> OldItem#rosterItem{ask = Value};
			jid -> OldItem#rosterItem{jid = Value};
			name -> OldItem#rosterItem{name = Value};
			subscription -> OldItem#rosterItem{subscription = Value};
			_ -> OldItem
		end
	end, #rosterItem{}, Item)
end, Items).

read_presence(Presence) -> lists:foldl(fun
	(#xmlElement{name = Name, content = Value}, OldPresence) -> case Name of
		show -> OldPresence#presence{show = read_text(Value)};
		status -> OldPresence#presence{status =
			[read_text(Value)|OldPresence#presence.status]};
		priority -> OldPresence#presence{priority = read_text(Value)};
		_ -> OldPresence
	end
end, #presence{}, Presence).

read_message(Message) -> lists:foldl(fun
	(#xmlElement{name = Name, content = Value}, OldMessage) -> case Name of
		subject -> OldMessage#message{subject = read_text(Value)};
		body -> OldMessage#message{body = read_text(Value)};
		_ -> OldMessage
	end
end, #message{}, Message).

read_stanza_error([?XmppStanzaErrorIn(Attributes, Content)|_]) -> lists:foldl(
	fun	(?XmppStanzaErrorTextIn(TextAttributes, Text), StanzaError) ->
			(read_stanza_error(TextAttributes, StanzaError))
				#stanzaError{text = read_text(Text)};
		(ApplicationCondition, StanzaError) -> StanzaError#stanzaError{
			application_condition = ApplicationCondition}
	end,
	read_stanza_error(Attributes, #stanzaError{condition = Condition}),
	Content
);
read_stanza_error([_|T]) -> read_stanza_error(T).

read_stanza_error(Attributes, Error) -> lists:foldl(fun
	(#xmlAttribute{name = Name, value = Value}, OldError) -> case Name of
		by -> OldError#stanzaError{by = Value};
		type -> OldError#stanzaError{type = Value};
		'xml:lang' -> OldError#stanzaError{lang = Value};
		_ -> OldError
	end
end, Error, Attributes).

read_stanza_attributes(Attributes) -> lists:foldl(fun
	(#xmlAttribute{name = Name, value = Value}, OldAttributes) -> case Name of
		id -> OldAttributes#stanzaAttributes{id = Value};
		from -> OldAttributes#stanzaAttributes{from = Value};
		to -> OldAttributes#stanzaAttributes{to = Value};
		type -> OldAttributes#stanzaAttributes{type = Value};
		'xml:lang' -> OldAttributes#stanzaAttributes{lang = Value}
	end
end, #stanzaAttributes{}, Attributes). 

read_vcard(VCard) -> lists:foldl(fun
	(#xmlElement{name = 'FN', content = Fn}, OldVCard) ->
		OldVCard#vCard{'FN' = read_text(Fn)};
	(_, OldVCard) -> OldVCard
end, #vCard{}, VCard).

read_optional(Optional) -> lists:map(fun	
	(#xmlElement{name = text, content = Content}) -> read_text(Content);
	(_) -> []
end, Optional).

read_text(Text) -> lists:flatten(lists:map(
	fun(#xmlText{value = Value}) -> Value end, Text)).

