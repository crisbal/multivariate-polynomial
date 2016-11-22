%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%% as_var_power/2
% this will parse a variable or variable^exponent expression and produce
% the correct representation v(exponent, variable)
% TODO add checks for atomic variable and exponent
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
% TODO: optimize so you don't predsort two times if SortedVPs==CompressedAndSortedVPs, after compress
as_monomial(Expression, m(Coefficient, TotalDegree, PerfectVarsPowers)) :-
	nonvar(Expression),
	!,
	as_monomial(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, CompressedAndSortedVPs),
	%sort again because of changes in the exponents in the join procedure
	predsort(compare_vars_powers, CompressedAndSortedVPs, PerfectVarsPowers).
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
as_monomial(OtherVars * CoefficientInTheMiddle, Coefficient, VarsPowers) :-
	% we handle coefficients that are in the middle of the monomial!
	number(CoefficientInTheMiddle),
	!,
	as_monomial(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInTheMiddle*OtherCoefficient.
as_monomial(OtherVars * Var, Coefficient, [VarPower | OtherVarPowers]) :-
	as_var_power(Var, VarPower),
	as_monomial(OtherVars, Coefficient, OtherVarPowers),
	!.
% TODO: add chacks for atomic coefficients
as_monomial(Coefficient, Coefficient, []) :-
	number(Coefficient),
	!.
as_monomial(HeadVarPower, 1, [VarPower]) :-
	as_var_power(HeadVarPower, VarPower),
	!.

%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% TODO: sort monomials
% TODO: two way
as_polynomial(Expression, p(SortedAndCompressedMonomials)) :-
	as_polynomial_parse(Expression, Monomials),
	!,
	predsort(compare_monomials, Monomials, SortedMonomials),
    compress_sorted_monomials(SortedMonomials,SortedAndCompressedMonomials).
	!.

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


coefficients(p(Monomials), Coefficients) :-
	coefficients_worker(Monomials, Coefficients).
coefficients_worker([], []).
coefficients_worker([m(Coefficient, _Degree, _VarsPowers) | RestOfMonomials], [Coefficient | RestOfCoefficients]) :-
	% TODO: add check for is_monomial
	coefficients_worker(RestOfMonomials, RestOfCoefficients).	

% TODO: improve checks to really be sure that this is a list of monomials
monomials(p(Monomials), Monomials).


%%% "helper"/not core rules

compress_sorted_vps([], []) :- !.
compress_sorted_vps([v(E,B)], [v(E,B)]) :- !.
compress_sorted_vps([v(E1, B), v(E2, B) | RestOfVps], Result) :-
	NewExp is E1+E2,
	compress_sorted_vps([v(NewExp, B) | RestOfVps], Result),
	!.
compress_sorted_vps([v(E1, B1), v(E2, B2) | RestOfVps], [v(E1, B1) | Result]) :-
	compress_sorted_vps([v(E2, B2) | RestOfVps], Result),
	!.



compute_total_degree_for_vars_powers([], 0) :- !.
compute_total_degree_for_vars_powers([v(Power,_)], Power) :- !.
compute_total_degree_for_vars_powers([v(Power,_)|Other], TotalDegree) :-
	compute_total_degree_for_vars_powers(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.

% this delta predicate is used by predsort/3 to sort vars powers
% this is really really useful since it takes away the need to write a sorting/looping algorithm
% and let us focus on the logic
% we don't provide an equality delta predicate (= is always false) since we cover all the cases with < and >
% This is so we keep duplicates, which are usually deleted by predsort  
compare_vars_powers(<, v(E1, _V1),v(E2, _V2)) :-
	E1>E2,
	!.
compare_vars_powers(<, v(E1, V1),v(E2, V2)) :-
	E1=E2,
	V1@=<V2, %<equals so we keep duplicates of the same variable, predosort usually deletes equals elements
	!.
compare_vars_powers(>, v(E1, _V1),v(E2, _V2)) :-
	E1<E2,
	!.
compare_vars_powers(>, v(E1, V1),v(E2, V2)) :-
	E1=E2,
	V1@>=V2, %>equals so we keep duplicates of the same variable
	!.


compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1>D2,
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1<D2,
	!.
% TODO: handle comparison of polys of same degree by comparing VPs
compare_monomials(=, m(C1, D1, VPs1), m(C2, D2, VPs2)) :-
	D1=D2,
	!.


%%% tests

compress_sorted_monomials([], []) :- !.
compress_sorted_monomials([m(C, T, V)], [m(C, T, V)]) :- !.
compress_sorted_monomials([m(C1, T, V), m(C2, T, V) | RestOfMonomials], Result) :-
		NewC is C1+C2,
		compress_sorted_monomials([m(NewC,T,V)|RestOfMonomials],Result),
		!.
compress_sorted_monomials([m(C1, T1, V1), m(C2, T2, V2) | RestOfMonomials], [m(C1, T1, V1)|Result]) :-
		compress_sorted_monomials([ m(C2, T2, V2) | RestOfMonomials],Result),
		!.

compare_monomials(>, m(_, TotalDegree1, _), m(_, TotalDegree2, _)) :-
				TotalDegree1 < TotalDegree2,
				!.
compare_monomials(>, m(Coefficient1, TotalDegree1, _), m(Coefficient2, TotalDegree2, _)) :-
				TotalDegree1 = TotalDegree2,
				Coefficient1 < Coefficient2,
				!.
compare_monomials(<, m(_, TotalDegree1, _), m(_, TotalDegree2, _)) :-
				TotalDegree1 >= TotalDegree2,
				!.
compare_monomials(<, m(Coefficient1, TotalDegree1, _), m(Coefficient2, TotalDegree2, _)) :-
				TotalDegree1 = TotalDegree2,
				Coefficient1 >= Coefficient2,
				!.

mvpoli_test :-
	load_test_files([]),
	run_tests.
