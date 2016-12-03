coefficients(GenericPoly, Coefficients) :-
    nonvar(GenericPoly),
    to_polynomial(GenericPoly, poly(Monomials)),
	is_polynomial(poly(Monomials)),
	coefficients_worker(Monomials, Coefficients).

coefficients_worker([], []).
coefficients_worker([m(Coefficient, _Degree, _VarsPowers) | RestOfMonomials], [Coefficient | RestOfCoefficients]) :-
	coefficients_worker(RestOfMonomials, RestOfCoefficients).

