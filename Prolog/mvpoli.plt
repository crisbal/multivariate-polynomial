%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%%%% Tests for mvpoli module
:- begin_tests(mvpoli).

test(as_var_power_1) :-
	mvpoli:as_var_power(x, v(1, x)).
test(as_var_power_2) :-
	mvpoli:as_var_power(x^2, v(2, x)).

test(as_monomial) :-
	mvpoli:as_monomial(42, m(42, 0, [])), % a number is a monomial
	mvpoli:as_monomial(21*2, m(42, 0, [])), % a number multiplied by a number is a monomial
	mvpoli:as_monomial(x^2, m(1, 2, [v(2,x)])), % a power is a monomial
	mvpoli:as_monomial(x^2*y^3, m(1, 5, [v(2,x), v(3,y)])), % simple monomial
	mvpoli:as_monomial(69*x^2*y^3, m(69, 5, [v(2,x), v(3,y)])), % monomial and a coefficient
	mvpoli:as_monomial(69*y^3*x^2*a^3, m(69, 8, [v(3,a), v(2,x), v(3,y)])), % test for alphabetical order of vps
	mvpoli:as_monomial(y^3*a^3*ab^3*a^6, m(1, 15, [v(9,a), v(3,ab), v(3,y)])), % same but for multiletter variables
	mvpoli:as_monomial(a*2*3*b^2, m(6, 3, [v(1, a), v(2, b)])), % two coefficients in the middle
	mvpoli:as_monomial(a*b*a^2*b^2, m(1, 6, [v(3, a), v(3, b)])). % repeating variables

test(as_polynomial) :-
	mvpoli:as_polynomial(42, poly([m(42, 0, [])])),
	mvpoli:as_polynomial(21 + 21, poly([m(42, 0, [])])),
	mvpoli:as_polynomial(21 + x + 21 + x + x, poly([m(3, 1, [v(1, x)]),m(42, 0, [])])),
	mvpoli:as_polynomial(-21 * (-2), poly([m(42, 0, [])])),
	mvpoli:as_polynomial(3*x- 4*x + 1, poly([m(-1, 1, [v(1, x)]), m(1, 0, [])])),
	mvpoli:as_polynomial(3*x - 4*x^2 + 1, poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])])),
	mvpoli:as_polynomial(a^2 + a^3, poly([m(1, 3, [v(3, a)]), m(1, 2, [v(2, a)])])),
	mvpoli:as_polynomial(x*y^2 + x^2*y + x^3, poly([m(1, 3, [v(3, x)]), m(1, 3, [v(2, x), v(1, y)]), m(1, 3, [v(1, x), v(2, y)])])).

test(coefficients) :-
	mvpoli:coefficients(poly([m(42, 0, [])]), [42]),
	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]),m(42, 0, [])]), [3,42]),
	mvpoli:coefficients(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [3, -4, 1]),
	mvpoli:coefficients(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [-4 ,3, 1]),
	mvpoli:coefficients(poly([m(1, 8, [v(5, y), v(2, x), v(1, z)]), m(1, 5, [v(3, z), v(1, x), v(1, y)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [1, 1, 1]).

test(variables) :-
	mvpoli:variables(poly([m(42, 0, [])]), []),
	mvpoli:variables(poly([m(3, 1, [v(1, x)]),m(42, 0, [])]), [x]),
	mvpoli:variables(poly([m(3, 1, [v(1, x)]), m(-4, 1, [v(1, x)]), m(1, 0, [])]), [x]),
	mvpoli:variables(poly([m(-4, 2, [v(2, x)]), m(3, 1, [v(1, x)]), m(1, 0, [])]), [x]),
	mvpoli:variables(poly([m(1, 8, [v(5, y), v(2, x), v(1, z)]), m(1, 5, [v(3, z), v(1, x), v(1, y)]), m(1, 3, [v(1, x), v(1, y), v(1, z)])]), [x, y, z]).

test(compute_total_degree_for_vars_powers) :-
	mvpoli:compute_total_degree_for_vars_powers([], 0),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x)], 2),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x), v(3, y)], 5).

:- end_tests(mvpoli).
