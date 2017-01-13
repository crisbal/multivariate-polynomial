%% compress_sorted_monomials/2
% same as compress_sorted_vps/2. but for the monomials
compress_sorted_monomials(I, R) :-
	nonvar(I),
	compress_sorted_monomials_real(I, R).

compress_sorted_monomials_real([], []) :- !.
compress_sorted_monomials_real([m(0, _, _) | RestOfMons], Result) :-
	compress_sorted_monomials_real(RestOfMons, Result),
	!.
compress_sorted_monomials_real([m(C, T, V)], [m(C, T, V)]) :- !.
compress_sorted_monomials_real([m(C1, T, V), m(C2, T, V) | RestOfMons], 
	Result) :-
	NewCoeff is C1+C2,
	compress_sorted_monomials_real([m(NewCoeff, T, V) | RestOfMons], Result),
	!.
compress_sorted_monomials_real([m(C1, T1, V1), m(C2, T2, V2) | RestOfMons], 
	[m(C1, T1, V1) | Result]) :-
	compress_sorted_monomials_real([ m(C2, T2, V2) | RestOfMons], Result),
	!.

