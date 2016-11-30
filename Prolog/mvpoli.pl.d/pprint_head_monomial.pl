pprint_head_monomial(m(Coefficient, _TD, VarsPowers)) :-
	write(Coefficient),
	pprint_vars_powers(VarsPowers),
	!.
