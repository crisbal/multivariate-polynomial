%% polyval/3
% evaluate the polynomial in the points
polyval(GenericPoly, VarValues, Result) :-
	nonvar(GenericPoly), % you can't go backward
	to_polynomial(GenericPoly, ReallyAPolynomial),
	variables(ReallyAPolynomial, VarSymbols),
	polyval_worker(ReallyAPolynomial, VarSymbols, VarValues, Result),
	!.

polyval_worker(poly([]), _, _, 0) :- !.
polyval_worker(poly([Monomial | RestOfMons]), VarSymbols, VarValues, Result) :-
	monoval(Monomial, VarSymbols, VarValues, MonomialResult),
	polyval_worker(poly(RestOfMons), VarSymbols, VarValues, RestOfMonsResult),
	Result is MonomialResult + RestOfMonsResult,
	!.
