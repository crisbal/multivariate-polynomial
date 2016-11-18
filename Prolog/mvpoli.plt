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

:- end_tests(mvpoli).