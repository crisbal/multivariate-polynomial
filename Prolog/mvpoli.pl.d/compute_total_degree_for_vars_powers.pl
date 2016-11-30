%% compute_total_degree_for_vars_powers/2
% this calculates the total degree of a monomial by passing the list of vps
% TODO: disallow two-way
compute_total_degree_for_vars_powers([], 0) :- !.
compute_total_degree_for_vars_powers([v(Power, _)], Power) :- !.
compute_total_degree_for_vars_powers([v(Power, _) | Other], TotalDegree) :-
	compute_total_degree_for_vars_powers(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.

