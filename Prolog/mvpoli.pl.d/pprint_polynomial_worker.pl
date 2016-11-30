pprint_polynomial_worker([]) :- !.
pprint_polynomial_worker([Monomial | OtherMonomials]) :-
	pprint_monomial(Monomial),
	pprint_polynomial_worker(OtherMonomials),
	!.
