%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%%%% Tests for mvpoli module
:- begin_tests(mvpoli).

test(as_var_power) :-
	mvpoli:as_var_power(x, v(1, x)),
	mvpoli:as_var_power(x^2, v(2, x)).

test(as_monomial) :-
	mvpoli:as_monomial(42, m(42, 0, [])),
	mvpoli:as_monomial(21*2, m(42, 0, [])),
	mvpoli:as_monomial(x^2, m(1, 2, [v(2,x)])),
	mvpoli:as_monomial(x^2*y^3, m(1, 5, [v(3,y), v(2,x)])),
	mvpoli:as_monomial(69*x^2*y^3, m(69, 5, [v(3,y), v(2,x)])),
	mvpoli:as_monomial(69*y^3*x^2, m(69, 5, [v(3,y), v(2,x)])).

test(as_polynomial) :-
	mvpoli:as_polynomial(42, p([m(42, 0, [])])),
	mvpoli:as_polynomial(-21*(-2), p([m(42, 0, [])])),
	mvpoli:as_polynomial(3*x-4*x+1, p([m(1, 0, []), m(-4, 1, [v(1, x)]), m(3, 1, [v(1, x)])])).

test(compute_total_degree_for_vars_powers) :-
	mvpoli:compute_total_degree_for_vars_powers([], 0),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x)], 2),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x), v(3, y)], 5).

test(sort_vars_powers) :-
	mvpoli:sort_vars_powers([], [],0),
	mvpoli:sort_vars_powers([], []),
	mvpoli:sort_vars_powers([v(2, x)], [v(2, x)],0),
	mvpoli:sort_vars_powers([v(2, x)], [v(2, x)]),
	mvpoli:sort_vars_powers([v(2, x),v(3, y)], [v(3, y),v(2, x)]),
	mvpoli:sort_vars_powers([v(2, x),v(3, y),v(4, z)], [v(4, z),v(3, y),v(2, x)]),
	mvpoli:sort_vars_powers([v(2, x),v(3, y),v(4, z),v(5, q)], [v(5, q),v(4, z),v(3, y),v(2, x)]),
	mvpoli:sort_vars_powers([v(3, x),v(2, y),v(4, z),v(5, q)], [v(5, q),v(4, z),v(3, x),v(2, y)]).


:- end_tests(mvpoli).
