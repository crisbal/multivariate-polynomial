%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%%%% Tests for mvpoli module
:- begin_tests(mvpoli).

test(is_var_power_1) :-
	mvpoli:is_var_power(v(2, x)).
test(is_var_power_2) :-
	mvpoli:is_var_power(v(1, y)).
test(is_var_power_2) :-
	mvpoli:is_var_power(v(1, zzz)).

test(as_var_power_1) :-
	mvpoli:as_var_power(x, v(1, x)).
test(as_var_power_2) :-
	mvpoli:as_var_power(x^2, v(2, x)).

test(as_monomial_1) :-
	mvpoli:as_monomial(42, m(42, 0, [])). % a number is a monomial
test(as_monomial_2) :-
	mvpoli:as_monomial(21*2, m(42, 0, [])). % a number multiplied by a number is a monomial
test(as_monomial_3) :-
	mvpoli:as_monomial(x^2, m(1, 2, [v(2,x)])). % a power is a monomial
test(as_monomial_4) :-
	mvpoli:as_monomial(x^2*y^3, m(1, 5, [v(2,x), v(3,y)])). % simple monomial
test(as_monomial_5) :-
	mvpoli:as_monomial(69*x^2*y^3, m(69, 5, [v(2,x), v(3,y)])). % monomial and a coefficient
test(as_monomial_6) :-
	mvpoli:as_monomial(69*y^3*x^2*a^3, m(69, 8, [v(3,a), v(2,x), v(3,y)])). % test for alphabetical order of vps
test(as_monomial_7) :-
	mvpoli:as_monomial(y^3*a^3*ab^3*a^6, m(1, 15, [v(9,a), v(3,ab), v(3,y)])). % same but for multiletter variables
test(as_monomial_8) :-
	mvpoli:as_monomial(a*2*3*b^2, m(6, 3, [v(1, a), v(2, b)])). % two coefficients in the middle
test(as_monomial_9) :-
	mvpoli:as_monomial(a*b*a^2*b^2, m(1, 6, [v(3, a), v(3, b)])). % repeating variables

test(as_polynomial_1) :-
	mvpoli:as_polynomial(42, poly([m(42, 0, [])])).
test(as_polynomial_2) :-
	mvpoli:as_polynomial(21 + 21, poly([m(42, 0, [])])).
test(as_polynomial_3) :-
	mvpoli:as_polynomial(21 + x + 21 + x + x, poly([m(3, 1, [v(1, x)]),m(42, 0, [])])).
test(as_polynomial_4) :-
	mvpoli:as_polynomial(-21 * (-2), poly([m(42, 0, [])])).
test(as_polynomial_5) :-
	mvpoli:as_polynomial(3*x- 4*x + 1, poly([m(-1, 1, [v(1, x)]), m(1, 0, [])])).
test(as_polynomial_6) :-
	mvpoli:as_polynomial(3*x - 4*x^2 + 1, poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])])).
test(as_polynomial_7) :-
	mvpoli:as_polynomial(a^2 + a^3, poly([m(1, 3, [v(3, a)]), m(1, 2, [v(2, a)])])).
test(as_polynomial_9) :-
	mvpoli:as_polynomial(x*y^2 + x^2*y + x^3, poly([m(1, 3, [v(3, x)]), m(1, 3, [v(2, x), v(1, y)]), m(1, 3, [v(1, x), v(2, y)])])).

test(coefficients_1) :-
	mvpoli:coefficients(poly([m(42, 0, [])]), [42]).
test(coefficients_2) :-
	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]),m(42, 0, [])]), [3,42]).
test(coefficients_3) :-
	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [3, -4, 1]).
test(coefficients_4) :-
	mvpoli:coefficients(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [-4 ,3, 1]).
test(coefficients_5) :-
	mvpoli:coefficients(poly([m(1, 8, [v(5, y), v(2, x), v(1, z)]), m(1, 5, [v(3, z), v(1, x), v(1, y)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [1, 1, 1]).

test(variables_1) :-
	mvpoli:variables(poly([m(42, 0, [])]), []).
test(variables_2) :-
	mvpoli:variables(poly([m(3, 1, [v(1, x)]),m(42, 0, [])]), [x]).
test(variables_3) :-
	mvpoli:variables(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [x]).
test(variables_4) :-
	mvpoli:variables(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [x]).
test(variables_5) :-
	mvpoli:variables(poly([m(1, 8, [v(5, y), v(2, x), v(1, z)]), m(1, 5, [v(3, z), v(1, x), v(1, y)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [x, y, z]).

test(compute_total_degree_for_vars_powers_1) :-
	mvpoli:compute_total_degree_for_vars_powers([], 0).
test(compute_total_degree_for_vars_powers_2) :-
	mvpoli:compute_total_degree_for_vars_powers([v(2, x)], 2).
test(compute_total_degree_for_vars_powers_3) :-
	mvpoli:compute_total_degree_for_vars_powers([v(2, x), v(3, y)], 5).

test(polyplus_1) :-
	mvpoli:as_polynomial(3*x, P1),
	mvpoli:as_polynomial(4*x, P2),
	mvpoli:as_polynomial(7*x, PR),
	mvpoli:polyplus(P1, P2, PR).
test(polyplus_2) :-
	mvpoli:as_polynomial(3*x, P1),
	mvpoli:as_polynomial(3*y, P2),
	mvpoli:as_polynomial(3*x+3*y, PR),
	mvpoli:polyplus(P1, P2, PR).
test(polyplus_3) :-
	mvpoli:as_polynomial(3*x+4*y, P1),
	mvpoli:as_polynomial(3*y ,P2),
	mvpoli:as_polynomial(3*x+7*y, PR),
	mvpoli:polyplus(P1, P2, PR).
test(polyplus_4) :-
	mvpoli:as_polynomial(3, P1),
	mvpoli:as_polynomial(39, P2),
	mvpoli:as_polynomial(42, PR),
	mvpoli:polyplus(P1, P2, PR).

test(polyminus_1) :-
	mvpoli:as_polynomial(3*x, P1),
	mvpoli:as_polynomial(4*x, P2),
	mvpoli:as_polynomial(-1*x, PR),
	mvpoli:polyminus(P1, P2, PR).

test(monomials_times_minus_one_1) :-
	mvpoli:as_polynomial(3*x, poly(M1)),
	mvpoli:as_polynomial(-3*x, poly(M2)),
	mvpoli:monomials_times_minus_one(M1, M2).
test(monomials_times_minus_one_2) :-
	mvpoli:as_polynomial(3, poly(M1)),
	mvpoli:as_polynomial(-3, poly(M2)),
	mvpoli:monomials_times_minus_one(M1, M2).

test(monotimes_1) :-
	mvpoli:as_monomial(3*x, M1),
	mvpoli:as_monomial(-3*x, M2),
	mvpoli:as_monomial(-9*x^2, MR),
	mvpoli:monotimes(M1, M2, MR).
test(monotimes_2) :-
	mvpoli:as_monomial(3, M1),
	mvpoli:as_monomial(-3, M2),
	mvpoli:as_monomial(-9, MR),
	mvpoli:monotimes(M1, M2, MR).

test(monotimespoly_1) :-
	mvpoli:as_monomial(3, M),
	mvpoli:as_polynomial(-3*x+3*y, P),
	mvpoli:as_polynomial(9*y-9*x, PR),
	mvpoli:monotimespoly(M, P, PR).

test(polytimes_1) :-
	mvpoli:as_polynomial(x^3, P1),
	mvpoli:as_polynomial(x, P2),
	mvpoli:as_polynomial(x^4, PR),
	mvpoli:polytimes(P1, P2, PR).
test(polytimes_2) :-
	mvpoli:as_polynomial(x^3, P1),
	mvpoli:as_polynomial(x^5, P2),
	mvpoli:as_polynomial(x^8, PR),
	mvpoli:polytimes(P1, P2, PR).
test(polytimes_3) :-
	mvpoli:as_polynomial(x^3+y, P1),
	mvpoli:as_polynomial(x^5, P2),
	mvpoli:as_polynomial(x^8+x^5*y, PR),
	mvpoli:polytimes(P1, P2, PR).
test(polytimes_4) :-
	mvpoli:as_polynomial(x^3+y, P1),
	mvpoli:as_polynomial(x^5+y, P2),
	mvpoli:as_polynomial(y*x^3+y*x^5+x^8+y^2, PR),
	mvpoli:polytimes(P1, P2, PR).
test(polytimes_5) :-
	mvpoli:as_polynomial(x^3+y+z, P1),
	mvpoli:as_polynomial(x^5+y, P2),
	mvpoli:as_polynomial(y*x^3+y*x^5+x^8+y^2+z*x^5+z*y, PR),
	mvpoli:polytimes(P1, P2, PR).

test(compress_sorted_vps_1) :-
	mvpoli:as_polynomial(3*x, P1),
	mvpoli:as_polynomial(3*x, P2),
	mvpoli:polyminus(P1, P2, poly(PR)),
	mvpoli:compress_sorted_vps(PR,[]).

test(compress_sorted_monomials_1) :-
	mvpoli:as_polynomial(3*x-3*x, poly(M)),
	M=[].
test(compress_sorted_monomials_2) :-
	mvpoli:as_polynomial(3*x-y-3*x+y, poly(M)),
	M=[].

:- end_tests(mvpoli).
