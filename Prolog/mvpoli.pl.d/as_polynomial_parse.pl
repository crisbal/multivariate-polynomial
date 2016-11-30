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
