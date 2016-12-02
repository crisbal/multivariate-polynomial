pprint_monomial(m(Coefficient, _TD, VarsPowers)) :-
	write(" + "),
	write(Coefficient),
	pprint_vars_powers(VarsPowers),
	!.

pprint_monomial_head(m(Coefficient, _TD, VarsPowers)) :-
	write(Coefficient),
	pprint_vars_powers(VarsPowers),
	!.

