%% to_polynomial/2
% these predicates will take care of casting a monomial or an Expression to a 
% poly() object.
to_polynomial(poly(Monomials), poly(SortedAndCompressedMonomials)) :-
	is_polynomial(poly(Monomials)),
	predsort(compare_monomials, Monomials, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, SortedAndCompressedMonomials),
	!.
to_polynomial(Monomial, poly([Monomial])) :-
	is_monomial(Monomial),
	!.
to_polynomial(Expression, Polynomial) :-
	nonvar(Expression),
	as_polynomial(Expression, Polynomial),
	!.

to_polynomial(Variable, Polynomial) :-
	var(Variable),
	is_polynomial(Polynomial),
	!.
