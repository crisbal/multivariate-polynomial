%% monoval/4
% evaluate the monomial in the points
monoval(I1, I2, I3, R) :-
	nonvar(I1),
	nonvar(I2),
	nonvar(I3),
	monoval_real(I1,I2,I3,R),
	!.
    
monoval_real(m(Coefficient, _TD, VPs), VarSymbols, VarValues, Result) :-
	varpowersval(VPs, VarSymbols, VarValues, VPsValue),
	Result is Coefficient*VPsValue,
	!.
