%% monoval/4
% evaluate the monomial in the points
% TODO: disallow two way
monoval(m(Coefficient, _TD, VPs), VarSymbols, VarValues, Result) :-
	varpowersval(VPs, VarSymbols, VarValues, VPsValue),
	Result is Coefficient*VPsValue,
	!.
