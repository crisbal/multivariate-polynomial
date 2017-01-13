%% monotimes/3
% do the polynomial multiplication between two monomials
monotimes(m(C1, T1, V1), m(C2, T2, V2), m(CR, TR, VR)) :-
	CR is C1 * C2,
	TR is T1 + T2,
	append(V1, V2, VarsPowers),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, VR).
