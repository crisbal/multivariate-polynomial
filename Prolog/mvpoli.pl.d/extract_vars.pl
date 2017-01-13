%% extract_vars/2
% this allows for extraction of variables from a list of VPs
extract_vars([], CurrentVars, CurrentVars).
extract_vars([v(_E, V) | RestOfVPs], CurrentVars, Answer) :-
	extract_vars(RestOfVPs, [V | CurrentVars], Answer).
