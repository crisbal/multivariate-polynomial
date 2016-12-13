%% compute_total_degree_for_vars_powers/2
% this calculates the total degree of a monomial by passing the list of vps
compute_total_degree_for_vars_powers(Powers,R) :-
	nonvar(Powers),
	compute_total_degree_for_vars_powers_real(Powers,R).
compute_total_degree_for_vars_powers_real([], 0) :- !.
compute_total_degree_for_vars_powers_real([v(Power, _)], Power) :- !.
compute_total_degree_for_vars_powers_real([v(Power, _) | Other], TotalDegree) :-
	compute_total_degree_for_vars_powers_real(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.

