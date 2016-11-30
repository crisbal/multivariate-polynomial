%% mindegree/2
% TODO: disallow two-way
mindegree(poly([m(_, TD, _)|Monomials]),Degree) :-
  mindegree_worker([m(_, TD, _)|Monomials],TD,Degree),
  !.

mindegree_worker([],CurrentMinDegree,CurrentMinDegree) :- !.
mindegree_worker([m(_, TD, _)|Monomials],CurrentMinDegree,Degree) :-
  TD < CurrentMinDegree,
  mindegree_worker(Monomials,TD,Degree).
mindegree_worker([m(_, _, _)|Monomials],CurrentMinDegree,Degree) :-
  mindegree_worker(Monomials,CurrentMinDegree,Degree).
