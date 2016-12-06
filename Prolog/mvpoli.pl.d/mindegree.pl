%% mindegree/2
% find mindegree of a polynomial
mindegree(Expression, Degree) :-
	nonvar(Expression),
	to_polynomial(Expression, P),
	mindegree_real(P, Degree).

mindegree_real(poly([]), 0) :- !.
mindegree_real(poly([m(_, TD, _) | Monomials]), Degree) :-
  mindegree_worker([m(_, TD, _) | Monomials], TD, Degree),
  !.

mindegree_worker([], CurrentMinDegree, CurrentMinDegree) :- !.
mindegree_worker([m(_, TD, _) | Monomials], CurrentMinDegree, Degree) :-
  TD < CurrentMinDegree,
  mindegree_worker(Monomials, TD, Degree).
mindegree_worker([m(_, _, _) | Monomials], CurrentMinDegree, Degree) :-
  mindegree_worker(Monomials, CurrentMinDegree, Degree).
