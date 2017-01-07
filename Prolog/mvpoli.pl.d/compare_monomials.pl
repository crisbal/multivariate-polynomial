%% compare_monomials/3
% this delta predicate is used by predsort/3 in as_polynomial, monomials are
% first compared by their TotalDegree then by degree+alphabetical of each 
% single VP. We keep "duplicates" since we will join them later on with 
% compress_sorted_monomials/3
compare_monomials(<, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 < D2,
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_order(VPs1, VPs2),
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.

compare_monomials(>, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 > D2,
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_order(VPs2, VPs1),
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.
