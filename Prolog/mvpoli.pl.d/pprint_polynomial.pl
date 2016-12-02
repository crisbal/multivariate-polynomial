pprint_polynomial(poly([])) :- pprint_polynomial_worker([]), !.
pprint_polynomial(poly([HeadMonomial | Monomials])) :-
	pprint_monomial_head(HeadMonomial),
	pprint_polynomial_worker(Monomials),
	!.

pprint_polynomial_worker([]) :- !.
pprint_polynomial_worker([Monomial | OtherMonomials]) :-
	pprint_monomial(Monomial),
	pprint_polynomial_worker(OtherMonomials),
	!.

