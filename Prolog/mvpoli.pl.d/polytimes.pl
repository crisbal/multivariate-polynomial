%% polytimes/3
% do polynomial multiplication between two polinomials
polytimes(GenericPoly1, GenericPoly2, poly(MonomialsResult)) :-
	nonvar(GenericPoly1),
	nonvar(GenericPoly2),
	to_polynomial(GenericPoly1, poly(M1)),
	to_polynomial(GenericPoly2, poly(M2)),
	polytimes_worker(M1, M2, Unsorted),
	predsort(compare_monomials, Unsorted, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, MonomialsResult).

%% polytimes_worker/3
% do the polynomial multiplication between two polinomials, the output will be 
% unsorted
polytimes_worker([], _, []) :- !.
polytimes_worker([MonHead | Monomials1], Monomials2, MonomialsR) :-
	monotimespoly(MonHead, poly(Monomials2), poly(MR)),
	polytimes_worker(Monomials1, Monomials2, MonomialsWorker),
	append(MR, MonomialsWorker, MonomialsR).

