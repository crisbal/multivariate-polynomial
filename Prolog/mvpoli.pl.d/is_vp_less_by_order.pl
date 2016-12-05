is_vp_less_by_order([v(_,V1) | _], [v(_,V2) | _]) :-
	V1 @< V2,
	!.
is_vp_less_by_order([v(E1,V1) | _], [v(E2,V2) | _]) :-
	V1 = V2,
	E1 < E2,
	!.
is_vp_less_by_order([v(E1,V1) | Resto1], [v(E2,V2) | Resto2]) :-
	V1 = V2,
	E1 = E2,
	is_vp_less_by_order(Resto1,Resto2),
	!.
