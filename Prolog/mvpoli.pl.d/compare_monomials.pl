%% compare_monomials/3
% this delta predicate is used by predsort/3 in as_polynomial, monomials are
% first compared by degree then by alphabetical order and then by power on a single letter. No comparison for
% coefficients is needed since we will take care of joining them with
% compress_sorted_monomials/3
compare_monomials(<, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 > D2,
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_alphabetical_order(VPs1, VPs2),
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_more_by_power_on_same_letter(VPs1, VPs2),
	!.
compare_monomials(<, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.
compare_monomials(>, m(_C1, D1, _VPs1), m(_C2, D2, _VPs2)) :-
	D1 < D2,
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_less_by_alphabetical_order(VPs2, VPs1),
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :-
	D1 = D2,
	is_vp_more_by_power_on_same_letter(VPs2, VPs1),
	!.
compare_monomials(>, m(_C1, D1, VPs1), m(_C2, D2, VPs2)) :- %to keep duplicates
	D1 = D2,
	VPs1 = VPs2,
	!.

