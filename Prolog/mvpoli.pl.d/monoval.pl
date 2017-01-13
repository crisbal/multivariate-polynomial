%% monoval/4
% evaluate the monomial in the points
monoval(Monomial, VarSymbols, VarValues, R) :-
	nonvar(Monomial),
	nonvar(VarSymbols),
	nonvar(VarValues),
	monoval_real(Monomial, VarSymbols, VarValues, R),
	!.
    
monoval_real(m(Coefficient, _TD, VPs), VarSymbols, VarValues, Result) :-
	varpowersval(VPs, VarSymbols, VarValues, VPsValue),
	Result is Coefficient * VPsValue,
	!.
