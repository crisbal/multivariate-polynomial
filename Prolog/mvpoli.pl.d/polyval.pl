%% polyval/3
% evaluate the polynomial in the points
% TODO: disallow two way
polyval(Polynomial, VarValues, Result) :-
	variables(Polynomial, VarSymbols),
	polyval_worker(Polynomial, VarSymbols, VarValues, Result),
	!.

% TODO: disallow two way
polyval_worker(poly([]), _, _, 0) :- !.
polyval_worker(poly([Monomial | RestOfMonomials]), VarSymbols, VarValues, Result) :-
	monoval(Monomial, VarSymbols, VarValues, MonomialResult),
	polyval_worker(poly(RestOfMonomials), VarSymbols, VarValues, RestOfMonomialsResult),
	Result is MonomialResult+RestOfMonomialsResult.

