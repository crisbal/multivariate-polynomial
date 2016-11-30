%% compress_sorted_vps/2
% the following rules "compress" a list of Vps. if there are two powers for the
% same variable one next to the other they will be merged together. It is sure
% that same variables will be one next to the other because we predsort the list
% before calling this
% TODO: disallow two-way
compress_sorted_vps([], []) :- !.
compress_sorted_vps([v(0, _) | RestOfVps], Result) :-
	compress_sorted_vps(RestOfVps , Result),
	!.
compress_sorted_vps([v(E, B)], [v(E, B)]) :- !.
compress_sorted_vps([v(E1, B), v(E2, B) | RestOfVps], Result) :-
	NewExp is E1+E2,
	compress_sorted_vps([v(NewExp, B) | RestOfVps], Result),
	!.
compress_sorted_vps([v(E1, B1), v(E2, B2) | RestOfVps], [v(E1, B1) | Result]) :-
	compress_sorted_vps([v(E2, B2) | RestOfVps], Result),
	!.

