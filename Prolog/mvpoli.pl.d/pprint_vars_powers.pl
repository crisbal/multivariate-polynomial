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
