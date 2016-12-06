%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% NOTE: we have to check if expression is a var or not, because if it is
% a var the program can't sort the VPs variable.
as_monomial(Expression, m(Coefficient, TotalDegree, FinalVPs)) :-
	nonvar(Expression),
	!,
	as_monomial_parse(Expression, Coefficient, VarsPowers),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, CompressedAndSortedVPs),
	handle_zero_coefficient(Coefficient, CompressedAndSortedVPs, FinalVPs),
	compute_total_degree_for_vars_powers(FinalVPs, TotalDegree).

as_monomial(Expression, m(Coefficient, TotalDegree, VarsPowers)) :-
	var(Expression),
	compress_sorted_vps(VarsPowers, CompressedVPs),
	reverse(CompressedVPs, ReversedVarsPowers), % so we print the m_vps in order
	as_monomial_parse(Expression, Coefficient, ReversedVarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	is_monomial(m(Coefficient, TotalDegree, VarsPowers)), % check if all is ok
	!.

% using the power of prolog we can write this parse function without
% doing too much parsing (as long as the input rules are respected)
% basically in this way we parse the polinomial backward from the end
% to the start, since order does not matter at this point we can do it without 
% problems. it works "backward" because of how the unificator works in prolog
as_monomial_parse(Coefficient, Coefficient, []) :-
	number(Coefficient),
	!.
as_monomial_parse(ArithmeticCoefficient, Coefficient, []) :-
	nonvar(ArithmeticCoefficient),
	% this will convert things like `sin(number)` without throwing exceptions
	arithmetic_expression_value(ArithmeticCoefficient, Coefficient),
	!.
as_monomial_parse(HeadVarPower, 1, [VarPower]) :-
	as_var_power(HeadVarPower, VarPower),
	!.
as_monomial_parse(-HeadVarPower, -1, [VarPower]) :-
	as_var_power(HeadVarPower, VarPower),
	!.
as_monomial_parse(OtherVars * CoefficientInside, Coefficient, VarsPowers) :-
	% we handle coefficients that are in the middle of the monomial!
	number(CoefficientInside),
	!,
	as_monomial_parse(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInside*OtherCoefficient.
as_monomial_parse(OtherVars * ArithmeticCoefficient, Coefficient, VarsPowers) :-
	nonvar(ArithmeticCoefficient),
	arithmetic_expression_value(ArithmeticCoefficient, CoefficientInside),
	!,
	as_monomial_parse(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInside*OtherCoefficient.
as_monomial_parse(OtherVars * Var, Coefficient, [VarPower | OtherVarPowers]) :-
	as_var_power(Var, VarPower),
	as_monomial_parse(OtherVars, Coefficient, OtherVarPowers),
	!.
as_monomial_parse(OtherVars * -Var, NegatedCoefficient, [VarPower | OtherVarPowers]) :-
	as_var_power(Var, VarPower),
	as_monomial_parse(OtherVars, Coefficient, OtherVarPowers),
	NegatedCoefficient is Coefficient*(-1),
	!.
