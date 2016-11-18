%%%% NUMBER SURNAME NAME
%%%% COLLABORATORS

%%%% Tests for mvpoli module
:- begin_tests(mvpoli).

test(as_monomial) :-
	mvpoli:as_monomial(42, m(42, 0, [])),
	mvpoli:as_monomial(21*2, m(42, 0, [])),
	mvpoli:as_monomial(x^2, m(1, 2, [v(2,x)])),
	mvpoli:as_monomial(x^2*y^3, m(1, 5, [v(3,y), v(2,x)])),
	mvpoli:as_monomial(69*x^2*y^3, m(69, 5, [v(3,y), v(2,x)])).

test(as_var_power) :-
	mvpoli:as_var_power(x, v(1, x)),
	mvpoli:as_var_power(x^2, v(2, x)).

test(compute_total_degree_for_vars_powers) :-
	mvpoli:compute_total_degree_for_vars_powers([], 0),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x)], 2),
	mvpoli:compute_total_degree_for_vars_powers([v(2, x), v(3, y)], 5).

:- end_tests(mvpoli).