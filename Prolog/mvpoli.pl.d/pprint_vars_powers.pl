pprint_vars_powers([]) :- !.
pprint_vars_powers([VP]) :- 
	pprint_VP(VP),
	!.
pprint_vars_powers([VP | OtherVarPowers]) :-
	pprint_VP(VP),
	write('*'),
	pprint_vars_powers(OtherVarPowers),
	!.

pprint_VP(v(1, Variable)) :-
	write(Variable),
	!.
pprint_VP(v(Exponent, Variable)) :-
	write(Variable),
	write('^'),
	write(Exponent),
	!.
