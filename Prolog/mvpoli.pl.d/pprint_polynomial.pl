pprint_polynomial(GenericPoly) :-
	to_polynomial(GenericPoly, Polynomial), 
	pprint_polynomial_real(Polynomial),
	!.

pprint_polynomial_real(poly([])) :- write(0).
% we write the head separately because there is no need print + and - here.
% FIXME: it's not the job fo pprint_monomial to print + and -
pprint_polynomial_real(poly([HeadMonomial | Monomials])) :-
	pprint_monomial_head(HeadMonomial), 
	pprint_polynomial_worker(Monomials),
	!.

pprint_polynomial_worker([]) :- !.
pprint_polynomial_worker([Monomial | OtherMonomials]) :-
	pprint_monomial(Monomial),
	pprint_polynomial_worker(OtherMonomials),
	!.

