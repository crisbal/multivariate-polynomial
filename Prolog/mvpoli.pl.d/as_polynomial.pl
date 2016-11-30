%% as_polynomial/2
% this is a wrapper for the function/engine that will parse the polynomial
% TODO: two way
as_polynomial(Expression, poly(SortedAndCompressedMonomials)) :-
	nonvar(Expression),
	as_polynomial_parse(Expression, Monomials),
	!,
	predsort(compare_monomials, Monomials, SortedMonomials),
	compress_sorted_monomials(SortedMonomials, SortedAndCompressedMonomials).
as_polynomial(Expression, poly(Monomials)) :-
	var(Expression),
	as_polynomial_parse(Expression, Monomials),
	!.
