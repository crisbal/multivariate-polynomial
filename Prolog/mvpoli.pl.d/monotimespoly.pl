%% monotimespoly/3
% do the polynomial multiplication between a polinomial and a monomial
monotimespoly(m(_, _, _), poly([]),	poly([])) :- !.
monotimespoly(m(C1, T1, V1), poly([m(C2, T2, V2) | Monomials]),
	poly([Monomial | MonomialsR])) :-
		monotimes(m(C1, T1, V1), m(C2, T2, V2), Monomial),
		monotimespoly(m(C1, T1, V1), poly(Monomials), poly(MonomialsR)).

