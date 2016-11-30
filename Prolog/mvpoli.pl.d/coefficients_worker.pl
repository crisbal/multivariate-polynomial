coefficients_worker([], []).
coefficients_worker([m(Coefficient, _Degree, _VarsPowers) | RestOfMonomials], [Coefficient | RestOfCoefficients]) :-
	% TODO: add check for is_monomial
	coefficients_worker(RestOfMonomials, RestOfCoefficients).
