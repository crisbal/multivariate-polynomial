%% polyplus/3
% do polynomial sum between two polinomials
polyplus(poly(Monomials1), poly(Monomials2), poly(MonomialsResult)) :-
	append(Monomials1, Monomials2, MonomialsAppend),
	predsort(compare_monomials, MonomialsAppend, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

