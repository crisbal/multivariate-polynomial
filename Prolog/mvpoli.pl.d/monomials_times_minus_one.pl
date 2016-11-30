%% monomials_times_minus_one/2
% do the polynomial multiplication between monomial and -1
monomials_times_minus_one([],[]) :-	!.
monomials_times_minus_one([m(C, T, V) | Monomials], [m(C_R, T, V) | Monomials_R]) :-
	C_R is -1*C,
	monomials_times_minus_one(Monomials, Monomials_R),
	!.

