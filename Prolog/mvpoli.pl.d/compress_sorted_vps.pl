%% compress_sorted_vps/2
% the following rules "compress" a list of Vps. if there are two powers for the
% same variable one next to the other they will be merged together. It is sure
% that same variables will be one next to the other because we predsort the list
% before calling this
compress_sorted_vps(I, R) :-
	nonvar(I),
	compress_sorted_vps_real(I, R).

compress_sorted_vps_real([], []) :- !.
compress_sorted_vps_real([v(0, _) | RestOfVps], Result) :-
	compress_sorted_vps_real(RestOfVps , Result),
	!.
compress_sorted_vps_real([v(E, B)], [v(E, B)]) :- !.
compress_sorted_vps_real([v(E1, B), v(E2, B) | RestOfVps], Result) :-
	NewExp is E1+E2,
	compress_sorted_vps_real([v(NewExp, B) | RestOfVps], Result),
	!.
compress_sorted_vps_real([v(E1, B1), v(E2, B2) | RestOfVps], 
        [v(E1, B1) | Result]) :-
	compress_sorted_vps_real([v(E2, B2) | RestOfVps], Result),
	!.

