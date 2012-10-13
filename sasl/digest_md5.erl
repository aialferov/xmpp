-module(digest_md5).
-export([digest_response/4, digest_response/5]).

-define(Qop, "auth").
-define(Charset, "utf-8").
-define(ServType, "xmpp").

-record(challenge, {realm = "", nonce = "", qop = ?Qop,
	charset = ?Charset, algorithm = "md-sess"}).

digest_response(UserName, Password, Host, Challenge) ->
	digest_response(UserName, Password, Host, "", Challenge).

digest_response(UserName, Password, Host, Service, Challenge) ->
	response(UserName, Password, Host, Service, read_challenge(Challenge)).

response(UserName, Password, Host, Service, Challenge) ->
	Cnonce = generate_cnonce(),
	NonceCount = string:right(integer_to_list(1, 16), 8, $0),
	DigestUri = ?ServType ++ "/" ++ Host ++
		case Service of "" -> ""; Service -> "/" ++ Service end,

	binary_to_list(base64:encode(
		"charset=" ++ ?Charset ++ "," ++
		"username=\"" ++ UserName ++ "\"," ++
		"realm=\"" ++ Challenge#challenge.realm ++ "\"," ++
		"nonce=\"" ++ Challenge#challenge.nonce ++ "\"," ++
		"cnonce=\"" ++ Cnonce ++ "\"," ++
		"nc=" ++ NonceCount ++ "," ++
		"qop=" ++ ?Qop ++ "," ++
		"digest-uri=\"" ++ DigestUri ++ "\"," ++
		"response=" ++ response(UserName, Password,
			Challenge#challenge.realm, Challenge#challenge.nonce,
			Cnonce, NonceCount, ?Qop, DigestUri
		)
	)).

response(UserName, Password, Realm,
	Nonce, Cnonce, NonceCount, Qop, DigestUri) ->

	A1 = h(UserName ++ ":" ++ Realm ++ ":" ++ Password) ++
		":" ++ Nonce ++ ":" ++ Cnonce,
	A2 = "AUTHENTICATE:" ++ DigestUri,
	hex(kd(hex(h(A1)), Nonce ++ ":" ++ NonceCount ++ ":" ++
		Cnonce ++ ":" ++ Qop ++ ":" ++ hex(h(A2)))).


read_challenge(Challenge) -> read_challenge(
	binary_to_list(base64:decode(Challenge)), #challenge{}).

read_challenge("realm=\"" ++ T, R) -> gather(realm, T, R);
read_challenge("nonce=\"" ++ T, R) -> gather(nonce, T, R);
read_challenge("qop=\"" ++ T, R) -> gather(qop, T, R);
read_challenge("charset=" ++ T, R) -> gather(charset, T, R);
read_challenge("algorithm=" ++ T, R) -> gather(algorithm, T, R);
read_challenge([_|T], R) -> read_challenge(T, R);
read_challenge([], R) -> R.

gather(Name, T, R) ->
	{Value, T1} = gather_tail(T, []),
	read_challenge(T1, setelement(challenge_field_index(Name), R, Value)).

gather_tail("\"" ++ T, L) -> {lists:reverse(L), T};
gather_tail("," ++ T, L) -> {lists:reverse(L), T};
gather_tail([H|T], L) -> gather_tail(T, [H|L]);                                 
gather_tail([], L) -> {lists:reverse(L), []}.

challenge_field_index(realm) -> #challenge.realm;
challenge_field_index(nonce) -> #challenge.nonce;
challenge_field_index(qop) -> #challenge.qop;
challenge_field_index(charset) -> #challenge.charset;
challenge_field_index(algorithm) -> #challenge.algorithm.

h(String) -> binary_to_list(crypto:md5(String)).
kd(HexK, HexD) -> h(HexK ++ ":" ++ HexD).
hex([H|T]) -> [digit(H bsr 4), digit(H band 16#f) | hex(T)];                    
hex([]) -> [].                                                                  
                                                                                
digit(N) when N < 10 -> $0 + N;                                                 
digit(N) -> $a + N - 10.

generate_cnonce() ->
	F = fun(X) -> hex(binary_to_list(crypto:rand_bytes(X))) end,
	lists:foldl(fun(X, []) -> F(X);
		(X, Acc) -> Acc ++ "-" ++ F(X) end, [], [4, 2, 2, 2, 6]).

