%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% NOTE: we have to check if expression is a var or not, because if it is
% a var the program will go out of stack (thanks to the sort), how could we fix?
% doing this is not so good but at least it does not break two-way unification
% it is mostly sort/2's (predsort) fault because you can't sort(R, [1, 2, 3]).
as_monomial(Expression, m(Coefficient, TotalDegree, CompressedAndSortedVPs)) :-
	nonvar(Expression),
	!,
	as_monomial_parse(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, CompressedAndSortedVPs).

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
% to the start, since order does not matter at this point we can do it!
% it works "backward" because of how the unificator works in prolog
% TODO: add checks for atomic coefficients
as_monomial_parse(Coefficient, Coefficient, []) :-
	number(Coefficient),
	!.
as_monomial_parse(ArithmeticCoefficient, Coefficient, []) :-
	nonvar(ArithmeticCoefficient),
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
as_monomial_parse(OtherVars * ArithmeticCoefficientInside, Coefficient, VarsPowers) :-
	nonvar(ArithmeticCoefficientInside),
	arithmetic_expression_value(ArithmeticCoefficientInside, CoefficientInside),
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