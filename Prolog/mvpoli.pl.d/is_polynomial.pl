%% is_polynomial/1
% TODO: do we have to check if the list is sorted?
is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)),
	!.

