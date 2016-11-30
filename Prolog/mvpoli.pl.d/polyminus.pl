%% polyminus/3
% do polynomial difference between two polinomials
polyminus(poly(Monomials1), poly(Monomials2), poly(MonomialsResult)) :-
	monomials_times_minus_one(Monomials2, InvertedMonomials),
	polyplus(poly(Monomials1), poly(InvertedMonomials), poly(MonomialsResult)).

