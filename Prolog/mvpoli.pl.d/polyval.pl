%% polyval/3
% evaluate the polynomial in the points
% TODO: disallow two way
polyval(Polynomial, VarValues, Result) :-
	variables(Polynomial, VarSymbols),
	polyval_worker(Polynomial, VarSymbols, VarValues, Result),
	!.
