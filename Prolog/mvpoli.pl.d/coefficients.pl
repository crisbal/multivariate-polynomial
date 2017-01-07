%% coefficients/2
% pretty straight forward, we extract the coefficients 
% of course we have to check if GenericPoly is a variable since there are 
% infinites polynomials that could have the passed coeffs. 
coefficients(GenericPoly, Coefficients) :-
    nonvar(GenericPoly), % you can't go backwards
    to_polynomial(GenericPoly, poly(Monomials)), % "cast" to polynomial type
	coefficients_worker(Monomials, Coefficients).

coefficients_worker([], []).
coefficients_worker([m(Coefficient, _Degree, _VarsPowers) | RestOfMonomials], 
        [Coefficient | RestOfCoefficients]) :-
	coefficients_worker(RestOfMonomials, RestOfCoefficients).

