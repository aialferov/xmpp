%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2012, Anton I Alferov
%%%
%%% Created : 13 Sep 2012 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(xmpp_reader).
-export([read_data/2]).

-include_lib("xmerl/include/xmerl.hrl").

-include("xmpp_utils.hrl").

-include("xmpp_core.hrl").
-include("xmpp_core_tools.hrl").

-include("xmpp_im.hrl").
-include("xmpp_im_tools.hrl").

-include("xmpp_im_obsolete.hrl").

-include("xmpp_ping.hrl").

-include("xmpp_vcard_temp.hrl").
-include("xmpp_vcard_temp_tools.hrl").

-include("xmpp_chat_state_notifications.hrl").
-include("xmpp_chat_state_notifications_tools.hrl").

read_data(Data, MoreDataMfa) ->
	case gather_tag(Data) of
		{ok, whitespace} -> {ok, whitespace};
		{ok, ?XmppStreamClosingTagName} -> {ok, stream_closed};
		{ok, ElementName} ->
			{Document, _Rest} = xmerl_scan:string(Data, [
				{continuation_fun, fun more_data/3,
					{MoreDataMfa, list_to_atom(ElementName)}},
				{hook_fun, fun switch_hook_state/2},
				{encoding, l1}
			]),
			{ok, build_response(Document)};
		{error, incomplete} -> case wait_more_data(MoreDataMfa) of
			{ok, MoreData} -> read_data(Data ++ MoreData, MoreDataMfa);
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

more_data({_, ?XmppStreamTagName}, ClosingTag) when
	ClosingTag == ?XmppStreamFeaturesTagName;
	ClosingTag == ?XmppStreamErrorTagName
-> {ok, ?XmppStreamClosingTag};
more_data({_, StartingTag}, ClosingTag)
	when StartingTag == ClosingTag -> {ok, done};
more_data({MoreDataMfa, _}, _) -> wait_more_data(MoreDataMfa).

wait_more_data({M, F, A}) -> apply(M, F, A).

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

	?XmppStreamHeaderIn([?XmppStreamErrorIn(Condition, Value, Optional)]) ->
		read_stream_error(Condition, Value, Optional);

	?XmppStreamErrorIn(Condition, Value, Optional) ->
		read_stream_error(Condition, Value, Optional);

	?XmppSaslChallengeIn(Content) -> {sasl_challenge, Content};

	?XmppSaslSuccessIn -> sasl_success;

	?XmppSaslFailueIn(Condition, Text) ->
		{sasl_failure, Condition, read_text(Text)};

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

read_stream_error(Condition, Value, Optional) -> #streamError{
	condition = #condition{name = Condition, value =
		case Condition of
			'see-other-host' -> read_host(read_text(Value));
			_ -> []
		end
	},
	optional = read_optional(Optional)
}.

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
read_stanza(iq, ?XepPing) -> ping;
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
	end;
	(_, OldPresence) -> OldPresence
end, #presence{}, Presence).

read_message(Message) -> lists:foldl(fun
	(?XepChatStateNotification(Type), Acc) -> Acc#message{extensions =
		[#chat_state_notification{type = Type}|Acc#message.extensions]};
	(#xmlElement{name = Name, content = Value}, Acc) -> case Name of
		subject -> Acc#message{subject = read_text(Value)};
		body -> Acc#message{body = read_text(Value)};
		_ -> Acc
	end
end, #message{}, Message).

read_stanza_error([?XmppStanzaErrorIn(
	Attributes, Condition, ConditionValue, Optional)|_]
) ->
	lists:foldl(
		fun(#xmlAttribute{name = Name, value = Value}, Error) -> case Name of
			by -> Error#stanzaError{by = Value};
			type -> Error#stanzaError{type = Value};
			_ -> Error
		end end,
		#stanzaError{
			condition = #condition{name = Condition,
				value = read_text(ConditionValue)},
			optional = read_optional(Optional)
		},
	Attributes);
read_stanza_error([_|T]) -> read_stanza_error(T).

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

read_optional(Optional) -> lists:foldl(fun
	(#xmlElement{name = text, content = Text}, OldOptional) ->
		OldOptional#optional{text = read_text(Text)};
	(_, OldOptional) -> OldOptional
end, #optional{}, Optional).

read_text(Text) -> lists:flatten(lists:map(
	fun(#xmlText{value = Value}) -> Value end, Text)).

read_host(Host) -> read_host(lists:reverse(Host), []).
read_host(Host = [$]|_], _Acc) -> {lists:reverse(Host), 0};
read_host([$:|T], Acc) -> {lists:reverse(T), list_to_integer(Acc)};
read_host([H|T], Acc) -> read_host(T, [H|Acc]);
read_host([], Acc) -> {Acc, 0}.
