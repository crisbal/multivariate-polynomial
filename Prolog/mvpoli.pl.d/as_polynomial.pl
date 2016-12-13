%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% the logic behind this is the same as as_monomial/2, we again check for nonvar 
% or var
as_polynomial(Expression, poly(SortedAndCompressedMonomials)) :-
	nonvar(Expression),
	as_polynomial_parse(Expression, Monomials),
	!,
	predsort(compare_monomials, Monomials, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, SortedAndCompressedMonomials).
as_polynomial(Expression, poly(Monomials)) :-
	var(Expression),
	reverse(Monomials, ReversedMonomials),
	as_polynomial_parse(Expression, ReversedMonomials),
	!.

%% as_polynomial_parse/2
% just like the monomials we parse, this time splitting by + and -
as_polynomial_parse(OtherExpr + MonExpr, [Mon | OtherMon]) :-
	as_monomial(MonExpr, Mon),
	as_polynomial_parse(OtherExpr, OtherMon),
	!.
as_polynomial_parse(OtherExpr - MonExpr, [m(NegCoeff, TotDeg, VPs) | OtherMon]) :-
	as_monomial(MonExpr, m(Coeff, TotDeg, VPs)),
	NegCoeff is Coeff*(-1),
	as_polynomial_parse(OtherExpr, OtherMon),
	!.
as_polynomial_parse(MonExpr, [Mon]) :-
	as_monomial(MonExpr, Mon),
	!.
as_polynomial_parse(-MonExpr, [m(NegCoeff, TotDeg, VPs)]) :- 
	as_monomial(MonExpr, m(Coeff, TotDeg, VPs)),
	NegCoeff is Coeff*(-1),
	!.
