% using the power of prolog we can write this parse function without
% doing too much parsing (as long as the input rules are respected)
% basically in this way we parse the polinomial backward from the end
% to the start, since order does not matter at this point we can do it!
% it works "backward" because of how the unificator works in prolog
as_monomial_parse(OtherVars * CoefficientInside, Coefficient, VarsPowers) :-
	% we handle coefficients that are in the middle of the monomial!
	number(CoefficientInside),
	!,
	as_monomial_parse(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInside*OtherCoefficient.
as_monomial_parse(OtherVars * Var, Coefficient, [VarPower | OtherVarPowers]) :-
	as_var_power(Var, VarPower),
	as_monomial_parse(OtherVars, Coefficient, OtherVarPowers),
	!.
% TODO: add checks for atomic coefficients
as_monomial_parse(Coefficient, Coefficient, []) :-
	number(Coefficient),
	!.
as_monomial_parse(HeadVarPower, 1, [VarPower]) :-
	as_var_power(HeadVarPower, VarPower),
	!.
