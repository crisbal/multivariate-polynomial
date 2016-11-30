%% polytimes/3
% do polynomial multiplication between two polinomials
polytimes(poly(M1), poly(M2), poly(MonomialsResult)) :-
	polytimes_worker(M1,M2,Unsorted),
	predsort(compare_monomials, Unsorted, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

