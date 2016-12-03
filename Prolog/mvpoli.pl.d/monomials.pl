% TODO: improve checks to really be sure that this is a list of monomials
monomials(GenericPoly, Monomials) :-
    to_polynomial(GenericPoly, poly(Monomials)),
    is_polynomial(poly(Monomials)).

