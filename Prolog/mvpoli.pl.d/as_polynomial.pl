%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% TODO: two way
as_polynomial(Expression, poly(SortedAndCompressedMonomials)) :-
	nonvar(Expression),
	as_polynomial_parse(Expression, Monomials),
	!,
	predsort(compare_monomials, Monomials, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, SortedAndCompressedMonomials).
as_polynomial(Expression, poly(Monomials)) :-
	var(Expression),
	as_polynomial_parse(Expression, Monomials),
	!.

%% as_polynomial_parse/2
% just like the monomials we parse, this time splitting by + and -
as_polynomial_parse(OtherMonExp + MonExp, [Mon | OtherMon]) :-
	as_monomial(MonExp, Mon),
	as_polynomial_parse(OtherMonExp, OtherMon),
	!.
as_polynomial_parse(OtherMonExp - MonExp, [m(NegCoeff, TotDeg, VPs) | OtherMon]) :-
	as_monomial(MonExp, m(Coeff, TotDeg, VPs)),
	NegCoeff is Coeff*(-1),
	as_polynomial_parse(OtherMonExp, OtherMon),
	!.
as_polynomial_parse(MonExp, [Mon]) :-
	as_monomial(MonExp, Mon),
	!.
as_polynomial_parse(-MonExp, [m(NegCoeff, TotDeg, VPs)]) :-
	as_monomial(MonExp, m(Coeff, TotDeg, VPs)),
	NegCoeff is Coeff*(-1),
	!.

