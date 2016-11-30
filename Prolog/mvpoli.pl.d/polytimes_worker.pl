%% polytimes_worker/3
% do the polynomial multiplication between two polinomials unsorted and not in a
% good layout
polytimes_worker([], _, []) :- !.
polytimes_worker([MonHead | Monomials1], Monomials2, MonomialsR) :-
	monotimespoly(MonHead, poly(Monomials2), poly(MR)),
	polytimes_worker(Monomials1, Monomials2, MonomialsWorker),
	append(MR,MonomialsWorker,MonomialsR).

