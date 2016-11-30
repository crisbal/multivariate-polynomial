pprint_polynomial(poly([])) :- pprint_polynomial_worker([]), !.
pprint_polynomial(poly([HeadMonomial | Monomials])) :-
	pprint_head_monomial(HeadMonomial),
	pprint_polynomial_worker(Monomials),
	!.

