%% maxdegree/2
maxdegree(E, Degree) :-
	nonvar(E),
	to_polynomial(E, P),
	maxdegree_real(P,Degree).

maxdegree_real(poly([]), 0) :- !.
maxdegree_real(poly([m(_, TD, _) | Monomials]), Degree) :-
	maxdegree_worker([m(_, TD, _) | Monomials], TD, Degree),
	!.

maxdegree_worker([], CurrentMaxDegree, CurrentMaxDegree) :- !.
maxdegree_worker([m(_, TD, _) | Monomials], CurrentMaxDegree, Degree) :-
	TD > CurrentMaxDegree,
	maxdegree_worker(Monomials, TD, Degree).
maxdegree_worker([m(_, _, _) | Monomials], CurrentMaxDegree, Degree) :-
	maxdegree_worker(Monomials, CurrentMaxDegree, Degree).
