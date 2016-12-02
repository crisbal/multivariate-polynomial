%% polyminus/3
% do polynomial difference between two polinomials
polyminus(Polynomial1, Polynomial2, poly(MonomialsResult)) :-
	to_polynomial(Polynomial1, poly(Monomials1)),
	to_polynomial(Polynomial2, poly(Monomials2)),
	monomials_times_minus_one(Monomials2, InvertedMonomials),
	polyplus(poly(Monomials1), poly(InvertedMonomials), poly(MonomialsResult)).

