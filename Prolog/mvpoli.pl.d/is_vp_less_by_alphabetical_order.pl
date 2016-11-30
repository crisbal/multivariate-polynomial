%% is_vp_lesser/2
% this helper predicate is used by compare_monomials/3 and "returns true" if the
% first list is "smaller" by comparing first by Variable ("smaller" first) and
% then by exponent (bigger first)
% TODO: disallow two-way
is_vp_less_by_alphabetical_order([], [v(_E, _V)]) :-
	!.
is_vp_less_by_alphabetical_order([v(_E1, V1) | _RestOfVps1], [v(_E2, V2) | _RestOfVps2]) :-
	V1 @< V2,
	!.
is_vp_less_by_alphabetical_order([v(_E1, V1) | RestOfVps1], [v(_E2, V2) | RestOfVps2]) :-
	V1 = V2,
	is_vp_less_by_alphabetical_order(RestOfVps1, RestOfVps2),
	!.

