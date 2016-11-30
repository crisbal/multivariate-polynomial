
%% as_monomial/2
% this is a wrapper for the engine that will parse as_monomial
% NOTE: we have to check if expression is a var or not, because if it is
% a var the program will go out of stack (thanks to the sort), how could we fix?
% doing this is not so good but at least it does not break two-way unification
% it is mostly sort/2's (predsort) fault because you can't sort(R, [1, 2, 3]).
as_monomial(Expression, m(Coefficient, TotalDegree, CompressedAndSortedVPs)) :-
	nonvar(Expression),
	!,
	as_monomial_parse(Expression, Coefficient, VarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	predsort(compare_vars_powers, VarsPowers, SortedVPs),
	compress_sorted_vps(SortedVPs, CompressedAndSortedVPs).

as_monomial(Expression, m(Coefficient, TotalDegree, VarsPowers)) :-
	var(Expression),
	compress_sorted_vps(VarsPowers, CompressedVPs),
	reverse(CompressedVPs, ReversedVarsPowers), % so we print the m_vps in order
	as_monomial_parse(Expression, Coefficient, ReversedVarsPowers),
	compute_total_degree_for_vars_powers(VarsPowers, TotalDegree),
	is_monomial(m(Coefficient, TotalDegree, VarsPowers)), % check if all is ok
	!.
