extract_vars([], CurrentVars, CurrentVars).
extract_vars([v(_E, V) | RestOfVPs], CurrentVars, Answer) :-
	extract_vars(RestOfVPs, [V | CurrentVars], Answer).

