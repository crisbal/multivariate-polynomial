%% to_polynomial/2
% these predicates will take care of casting a monomial or an Expression to a 
% poly() object.
to_polynomial(poly(Monomials), poly(Monomials)) :-
	is_polynomial(poly(Monomials)),
	!.
to_polynomial(Monomial, poly([Monomial])) :-
	is_monomial(Monomial),
	!.
to_polynomial(Expression, Polynomial) :-
	as_polynomial(Expression, Polynomial),
	!.