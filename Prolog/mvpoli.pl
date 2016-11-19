%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%% as_var_power/2
% this will parse a variable or variable^exponent expression and produce
% the correct representation v(exponent, variable)
as_var_power(Variable^Exponent, v(Exponent, Variable)) :-
	number(Exponent),
	!.
as_var_power(Variable, v(1, Variable)) :- !.


%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% FIXME: HACK: we have to check if expression is a var or not, because if it is
% a var the program will go out of stack (thanks to the sort), how could we fix?
% doing this is not so good but at least it does not break two-way unification
% we know it is sort/2's fault because you can't `sort(R, [1,2,3]).`
% FIXME: it sort VPs of the same power backward, if we use sort there is nothing
% we can change without reinventing the wheel, one option would be to use
% predsort/3 but it is not ISO prolog but only swi built-in.
as_monomial(Expression, m(Coefficient, TotalDegree, SortedVPs)) :-
	nonvar(Expression),
	!,
	as_monomial(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	sort_vars_powers(VarsPowers, SortedVPs).
as_monomial(Expression, m(Coefficient, TotalDegree, VarsPowers)) :-
	var(Expression),
	as_monomial(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	!.

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

%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% TODO: sort monomials
as_polynomial(Expression, p(Monomials)) :-
	as_polynomial_parse(Expression, Monomials).

%% as_polynomial_parse/2
% as for the monomials we work on this
as_polynomial_parse(OtherMonExp + MonExp, [Mon|OtherMon]) :-
	as_monomial(MonExp, Mon),
	as_polynomial_parse(OtherMonExp, OtherMon),
	!.
as_polynomial_parse(OtherMonExp - MonExp, [m(NegCoeff, TotDeg, VPs) | OtherMon]) :-
	as_monomial(MonExp, m(Coeff, TotDeg, VPs)),
	NegCoeff is Coeff*(-1),
	!,
	as_polynomial_parse(OtherMonExp, OtherMon).
as_polynomial_parse(MonExp, [Mon]) :-
	as_monomial(MonExp, Mon),
	!.


%%% "helper"/not core rules

compute_total_degree_for_vars_powers([], 0) :- !.
compute_total_degree_for_vars_powers([v(Power,_)], Power) :- !.
compute_total_degree_for_vars_powers([v(Power,_)|Other], TotalDegree) :-
	compute_total_degree_for_vars_powers(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.

%% rsort/2
% reverse sort a list
% http://stackoverflow.com/questions/4187632/fast-reverse-sort-in-swi-prolog
% ^ link above for more efficient solutions that we don't really need
reverse_sort(List, ReverseSorted) :-
    sort(List, Tmp),
    reverse(Tmp, ReverseSorted).

%% sort_vars_powers/2
% sort powers by the exponent, this the public interface
% The first argument is the input list of powers in the
%  format of [v(Power,Variable),...]
% The second argument is the output list sorted
sort_vars_powers(In,Out) :-
 		sort_vars_powers(In,NewList,C),
		C=0,
		Out=NewList.
sort_vars_powers(In,Out) :-
		sort_vars_powers(In,NewList,C),
		C\=0,
		sort_vars_powers(NewList,Out).

%% sort_vars_powers/3
% sort powers by the exponent and returns also how many swap has done
% The first argument is the input list of powers in the format
%  of [v(Power,Variable),...]
% The second argument is the output list sorted
% The third argument is the counter ot changes done
% FIXME: alphabetical order of the names of the variables
%  in the case of equal power value
% [v(2,b),v(2,a)] must be [v(2,a),v(2,b)]
sort_vars_powers([],[],0).
sort_vars_powers([Power],[Power],0).
sort_vars_powers([First|Tail],[First|OtherOutput],OtherC) :-
    Tail = [Second|Other],
    First = v(FirstExponent,_FirstSymbol),
    Second = v(SecondExponent,_SecondSymbol),
    FirstExponent > SecondExponent,
    sort_vars_powers([Second|Other],OtherOutput,OtherC).
sort_vars_powers([First|Tail],[Second|OtherOutput],C) :-
    Tail = [Second|Other],
    First = v(FirstExponent,_FirstSymbol),
    Second = v(SecondExponent,_SecondSymbol),
    not(FirstExponent > SecondExponent),
    sort_vars_powers([First|Other],OtherOutput,OtherC),
    C is OtherC + 1.


mvpoli_test :-
	load_test_files([]),
	run_tests.
