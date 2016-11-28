%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%% is_var_power/1
% FIXME? do we really have to check if the exponent is negative??
is_var_power(v(Exponent, Variable)) :-
	integer(Exponent),
	Exponent >= 0, %TODO: why?
	atomic(Variable).

%% as_var_power/2
% this will parse a variable or variable^exponent expression and produce
% the correct representation v(exponent, variable)
as_var_power(Variable^Exponent, v(Exponent, Variable)) :-
	is_var_power(v(Exponent, Variable)),
	!.
as_var_power(Variable, v(1, Variable)) :- 
	is_var_power(v(1, Variable)),
	!.
/*as_var_power(Base^Other, v(NewExponent, BaseVariable)) :-
	integer(Exponent),
	as_var_power(Other, v(OtherExponent, BaseVariable)),
	NewExponent is Exponent*OtherExponent,
	!.
*/

%% is_monomial/1
% TODO: add tests
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD>=0, %TODO: why?
	compute_total_degree_for_vars_powers(VPs, TD), %TODO: is needed?
	is_list(VPs),
	predsort(compare_vars_powers, VPs, VPs), %TODO: is needed?
	compress_sorted_vps(VPs, VPs), %TODO: is needed
	!. %TODO: is needed?

%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% NOTE: we have to check if expression is a var or not, because if it is
% a var the program will go out of stack (thanks to the sort), how could we fix?
% doing this is not so good but at least it does not break two-way unification
% we know it is sort/2's (or predsort) fault because you can't `sort(R, [1, 2, 3]).`
as_monomial(Expression, m(Coefficient, TotalDegree, CompressedAndSortedVPs)) :-
	nonvar(Expression),
	!,
	as_monomial_parse(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, CompressedAndSortedVPs).

as_monomial(Expression, m(Coefficient, TotalDegree, VarsPowers)) :-
	var(Expression),
	as_monomial_parse(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	!.

% using the power of prolog we can write this parse function without
% doing too much parsing (as long as the input rules are respected)
% basically in this way we parse the polinomial backward from the end
% to the start, since order does not matter at this point we can do it!
% it works "backward" because of how the unificator works in prolog
as_monomial_parse(OtherVars * CoefficientInTheMiddle, Coefficient, VarsPowers) :-
	% we handle coefficients that are in the middle of the monomial!
	number(CoefficientInTheMiddle),
	!,
	as_monomial_parse(OtherVars, OtherCoefficient, VarsPowers),
	Coefficient is CoefficientInTheMiddle*OtherCoefficient.
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


%% is_polynomial/1
% TODO: add tests
is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)),
	!.

%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% TODO: two way
as_polynomial(Expression, poly(SortedAndCompressedMonomials)) :-
	nonvar(Expression),
	as_polynomial_parse(Expression, Monomials),
	!,
	predsort(compare_monomials, Monomials, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, SortedAndCompressedMonomials).
as_polynomial(Expression, poly(Monomials)) :-
	var(Expression),
	as_polynomial_parse(Expression, Monomials),
	!.
%% as_polynomial_parse/2
% just like the monomials we parse, this time splitting by + and -
as_polynomial_parse(OtherMonExp + MonExp, [Mon | OtherMon]) :-
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

pprint_polynomial(poly(Monomials)) :-
	pprint_polynomial_worker(Monomials).
pprint_polynomial_worker([]) :- !.
pprint_polynomial_worker([Monomial | OtherMonomials]) :-
	pprint_monomial(Monomial),
	pprint_polynomial_worker(OtherMonomials),
	!.

pprint_monomial(m(Coefficient, _TD, VarsPowers)) :-
	write(" + "),
	write(Coefficient),
	pprint_vars_powers(VarsPowers),
	!.

pprint_vars_powers([]).
pprint_vars_powers([v(1, Variable) | OtherVarPowers]) :-
	write(" * "),
	write(Variable),
	pprint_vars_powers(OtherVarPowers).
pprint_vars_powers([v(Exponent, Variable) | OtherVarPowers]) :-
	write(" * "),
	write(Variable),
	write("^"),
	write(Exponent),
	pprint_vars_powers(OtherVarPowers).


coefficients(poly(Monomials), Coefficients) :-
	coefficients_worker(Monomials, Coefficients).
coefficients_worker([], []).
coefficients_worker([m(Coefficient, _Degree, _VarsPowers) | RestOfMonomials], [Coefficient | RestOfCoefficients]) :-
	% TODO: add check for is_monomial
	coefficients_worker(RestOfMonomials, RestOfCoefficients).

variables(poly(Monomials), UniqueAndSorted) :-
	variables_worker(Monomials, [], Variables),
	sort(Variables, UniqueAndSorted). %sort already removes duplicates, so we are good
variables_worker([], CurrentVars, CurrentVars).
variables_worker([m(_Coefficient, _Degree, VarsPowers) | RestOfMonomials], CurrentVars, Answer) :-
	% TODO: add check for is_monomial
	extract_vars(VarsPowers, CurrentVars, NewCurrentVars),
	variables_worker(RestOfMonomials, NewCurrentVars, Answer).

extract_vars([], CurrentVars, CurrentVars).
extract_vars([v(_E, V) | RestOfVPs], CurrentVars, Answer) :-
	extract_vars(RestOfVPs, [V | CurrentVars], Answer).

% TODO: improve checks to really be sure that this is a list of monomials
monomials(poly(Monomials), Monomials).

% FIXME: add comments
polyplus(poly(Monomials1), poly(Monomials2), poly(MonomialsResult)) :-
	append(Monomials1, Monomials2, MonomialsAppend),
	predsort(compare_monomials, MonomialsAppend, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

% FIXME: add comments
polyminus(poly(Monomials1), poly(Monomials2), poly(MonomialsResult)) :-
	monomials_times_minus_one(Monomials2,Monomials2_tmp),
	polyplus(poly(Monomials1),poly(Monomials2_tmp),poly(MonomialsResult)).

% FIXME: add comments
polytimes(poly(M1), poly(M2), poly(MonomialsResult)) :-
	polytimes_worker(M1,M2,Unsorted),
	predsort(compare_monomials, Unsorted, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

% FIXME: add comments
polytimes_worker([], _, []) :- !.
polytimes_worker([MonHead|Monomials1], Monomials2, MonomialsR) :-
	monotimespoly(MonHead, poly(Monomials2), poly(MR)),
	polytimes_worker(Monomials1, Monomials2, MonomialsWorker),
	append(MR,MonomialsWorker,MonomialsR).

%%% "helper"/not core rules
% FIXME: add comments
monotimespoly(m(_, _, _), poly([]),	poly([])) :- !.
monotimespoly(m(C1, T1, V1), poly([m(C2, T2, V2)|Monomials]),
	poly([Monomial|MonomialsR])) :-
		monotimes(m(C1, T1, V1), m(C2, T2, V2), Monomial),
		monotimespoly(m(C1, T1, V1), poly(Monomials), poly(MonomialsR)).

% FIXME: add comments
monotimes(m(C1, T1, V1), m(C2, T2, V2), m(CR, TR, VR)) :-
	CR is C1*C2,
	TR is T1+T2,
	append(V1, V2, VarsPowers),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, VR).

% FIXME: add comments
monomials_times_minus_one([],[]) :-	!.
monomials_times_minus_one([m(C, T, V)|Monomials],[m(C_R, T, V)|Monomials_R]) :-
	C_R is -1*C,
	monomials_times_minus_one(Monomials,Monomials_R),
	!.

%% compress_sorted_vps/2
% the following rules "compress" a list of Vps. if there are two powers for the
% same variable one next to the other they will be merged together. It is sure
% that same variables will be one next to the other because we predsort the list
% before calling this
% TODO: disallow two-way
% TODO: delete monomials with 0 coefficient
compress_sorted_vps([], []) :- !.
compress_sorted_vps([v(E, B)], [v(E, B)]) :- !.
compress_sorted_vps([v(E1, B), v(E2, B) | RestOfVps], Result) :-
	NewExp is E1+E2,
	compress_sorted_vps([v(NewExp, B) | RestOfVps], Result),
	!.
compress_sorted_vps([v(E1, B1), v(E2, B2) | RestOfVps], [v(E1, B1) | Result]) :-
	compress_sorted_vps([v(E2, B2) | RestOfVps], Result),
	!.

%% compress_sorted_monomials/2
% same as compress_sorted_vps/2. but for the monomials
% TODO: disallow two-way
compress_sorted_monomials([], []) :- !.
compress_sorted_monomials([m(C, T, V)], [m(C, T, V)]) :- !.
compress_sorted_monomials([m(C1, T, V), m(C2, T, V) | RestOfMons], Result) :-
	NewCoeff is C1+C2,
	compress_sorted_monomials([m(NewCoeff, T, V) | RestOfMons], Result),
	!.
compress_sorted_monomials([m(C1, T1, V1), m(C2, T2, V2) | RestOfMons], [m(C1, T1, V1) | Result]) :-
	compress_sorted_monomials([ m(C2, T2, V2) | RestOfMons], Result),
	!.


%% compute_total_degree_for_vars_powers/2
% this calculates the total degree of a monomial by passing the list of vps
% TODO: disallow two-way
compute_total_degree_for_vars_powers([], 0) :- !.
compute_total_degree_for_vars_powers([v(Power, _)], Power) :- !.
compute_total_degree_for_vars_powers([v(Power, _) | Other], TotalDegree) :-
	compute_total_degree_for_vars_powers(Other, OtherTotalDegree),
	TotalDegree is OtherTotalDegree+Power,
	!.

%% compare_vars_powers/3
% this delta predicate is used by predsort/3 to sort vars powers
% this is really really useful since it takes away the need to write a
% sorting/looping algorithm and let us focus on the logic
% we don't provide an equality delta predicate (= is always false) since we
% cover all the cases with < and >. This is so we keep duplicates, which are
% usually deleted by predsort. compress_sorted_vps/3 will take care of the rest
compare_vars_powers(<, v(_E1, V1), v(_E2, V2)) :-
	V1 @=< V2, %=< so we keep duplicates of the same variable
	!.
compare_vars_powers(>, v(_E1, V1), v(_E2, V2)) :-
	V1 @>= V2, %>= so we keep duplicates of the same variable
	!.

%% compare_monomials/3
% this delta predicate is used by predsort/3 in as_polynomial, monomials are
% first compared by degree then by alphabetical order and then by power on a single letter. No comparison for
% coefficients is needed since we will take care of joining them with
% compress_sorted_monomials/3
compare_monomials(<, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 > D2,
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_alphabetical_order(VPs1, VPs2),
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_more_by_power_on_same_letter(VPs1, VPs2),
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.

compare_monomials(>, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 < D2,
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_alphabetical_order(VPs2, VPs1),
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_more_by_power_on_same_letter(VPs2, VPs1),
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.

%% is_vp_lesser/2
% this helper predicate is used by compare_monomials/3 and "returns true" if the
% first list is "smaller" by comparing first by Variable ("smaller" first) and
% then by exponent (bigger first)
% TODO: disallow two-way
is_vp_less_by_alphabetical_order([], [v(_E, _V)]) :-
	!.
is_vp_less_by_alphabetical_order([v(_E1, V1) | _RestOfVps1], [v(_E2, V2) | _RestOfVps2]) :-
	V1 @< V2,
	!.
is_vp_less_by_alphabetical_order([v(_E1, V1) | RestOfVps1], [v(_E2, V2) | RestOfVps2]) :-
	V1 = V2,
	is_vp_less_by_alphabetical_order(RestOfVps1, RestOfVps2),
	!.

is_vp_more_by_power_on_same_letter([v(E1, V1) | _RestOfVps1], [v(E2, V2) | _RestOfVps2]) :-
	V1 = V2,
	E1 > E2,
	!.
is_vp_more_by_power_on_same_letter([v(E1, V1) | RestOfVps1], [v(E2, V2) | RestOfVps2]) :-
	V1 = V2,
	E1 = E2,
	is_vp_more_by_power_on_same_letter(RestOfVps1, RestOfVps2),
	!.

%%% tests
mvpoli_test :-
	load_test_files([]),
	run_tests.
