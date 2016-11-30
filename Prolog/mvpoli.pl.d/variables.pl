variables(poly(Monomials), UniqueAndSorted) :-
	variables_worker(Monomials, [], Variables),
	sort(Variables, UniqueAndSorted). %sort already removes duplicates, so we are good
