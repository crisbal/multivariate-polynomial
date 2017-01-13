% print + before the monomial
pprint_monomial(m(Coefficient, _TD, VarsPowers)) :-
	Coefficient >= 0,
	write(' + '),
	pprint_monomial_coefficient(Coefficient, VarsPowers),
	pprint_vars_powers(VarsPowers),
	!.

%print - before the monomial
pprint_monomial(m(Coefficient, _TD, VarsPowers)) :-
	Coefficient < 0,
	write(' - '),
	NegatedCoefficient is Coefficient * (-1), %negate coeff for the - before it
	pprint_monomial_coefficient(NegatedCoefficient, VarsPowers),
	pprint_vars_powers(VarsPowers),
	!.

pprint_monomial_head(m(Coefficient, _TD, VarsPowers)) :-
	pprint_monomial_coefficient(Coefficient, VarsPowers),
	pprint_vars_powers(VarsPowers),
	!.

pprint_monomial_coefficient(1, []) :- 
	write(1),
	!.
pprint_monomial_coefficient(1, _) :- !.
pprint_monomial_coefficient(Coefficient, []) :- 
	write(Coefficient),
	!.
pprint_monomial_coefficient(Coefficient, _VarsPowers) :- 
	write(Coefficient),
	write('*'),
	!.
