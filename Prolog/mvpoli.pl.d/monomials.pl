% TODO: check order?
monomials(GenericPoly, Monomials) :-
    to_polynomial(GenericPoly, poly(Monomials)),
    is_polynomial(poly(Monomials)). % TODO: remove? implicit in to_polynomial

