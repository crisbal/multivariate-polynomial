%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% TODO: Sort VarsPowers
as_monomial(Expression, m(Coefficient, TotalDegree, VarsPowers)) :-
	as_monomial(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree).

% using the power of prolog we can write this parse function without
% doing too much parsing (as long as the input rules are respected)
% basically in this way we parse the polinomial backward from the end
% to the start, since order does not matter at this point we can do it!
% it works "backward" because of how the unificator works in prolog

% we handle coefficients that are in the middle of the monomial!
% (this is beautiful!)
as_monomial(OtherVars * CoefficientInTheMiddle, Coefficient, VarsPowers) :-
	number(CoefficientInTheMiddle),
	!,
	as_monomial(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInTheMiddle*OtherCoefficient.
as_monomial(OtherVars * Var, Coefficient, [VarPower | OtherVarPowers]) :-
	as_var_power(Var, VarPower),
	as_monomial(OtherVars, Coefficient, OtherVarPowers),
	!.
as_monomial(Coefficient, Coefficient, []) :-
	number(Coefficient),
	!.
as_monomial(HeadVarPower, 1, [VarPower]) :-
	as_var_power(HeadVarPower, VarPower),
	!.

as_var_power(Variable^Exponent, v(Exponent, Variable)) :-
	number(Exponent),
	!.
as_var_power(Variable, v(1, Variable)) :- !.


compute_total_degree_for_vars_powers([], 0) :- !.
compute_total_degree_for_vars_powers([v(Power,_)], Power) :- !.
compute_total_degree_for_vars_powers([v(Power,_)|Other], TotalDegree) :-
	compute_total_degree_for_vars_powers(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.


mvpoli_test :-
	load_test_files([]),
	run_tests.