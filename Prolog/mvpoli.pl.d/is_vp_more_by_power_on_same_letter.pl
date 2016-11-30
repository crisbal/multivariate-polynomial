is_vp_more_by_power_on_same_letter([v(E1, V1) | _RestOfVps1], [v(E2, V2) | _RestOfVps2]) :-
	V1 = V2,
	E1 > E2,
	!.
is_vp_more_by_power_on_same_letter([v(E1, V1) | RestOfVps1], [v(E2, V2) | RestOfVps2]) :-
	V1 = V2,
	E1 = E2,
	is_vp_more_by_power_on_same_letter(RestOfVps1, RestOfVps2),
	!.

