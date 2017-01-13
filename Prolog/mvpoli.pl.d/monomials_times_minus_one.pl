%% monomials_times_minus_one/2
% do the polynomial multiplication between a list of monomials and -1
monomials_times_minus_one([],[]) :-	!.
monomials_times_minus_one([m(C, T, V) | Monomials], 
	[m(C_R, T, V) | Monomials_R]) :-
	C_R is C * (-1),
	monomials_times_minus_one(Monomials, Monomials_R),
	!.
