%% polyplus/3
% do polynomial sum between two polinomials
% pretty simple, just concat the list of monomials, sort and compress.
polyplus(Polynomial1, Polynomial2, poly(MonomialsResult)) :-
	to_polynomial(Polynomial1, poly(Monomials1)),
	to_polynomial(Polynomial2, poly(Monomials2)),
	append(Monomials1, Monomials2, MonomialsAppend),
	predsort(compare_monomials, MonomialsAppend, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

