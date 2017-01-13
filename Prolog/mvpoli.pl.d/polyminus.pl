%% polyminus/3
% do polynomial difference between two polinomials
% it is just a polyplus but negate the first monomial list
polyminus(Polynomial1, Polynomial2, poly(MonomialsResult)) :-
	nonvar(Polynomial1),
	nonvar(Polynomial2),
	to_polynomial(Polynomial1, poly(Monomials1)),
	to_polynomial(Polynomial2, poly(Monomials2)),
	monomials_times_minus_one(Monomials2, InvertedMonomials),
	polyplus(poly(Monomials1), poly(InvertedMonomials), poly(MonomialsResult)).
