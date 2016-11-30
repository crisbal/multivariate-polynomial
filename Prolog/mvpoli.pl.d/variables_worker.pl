variables_worker([], CurrentVars, CurrentVars).
variables_worker([m(_Coefficient, _Degree, VarsPowers) | RestOfMonomials], CurrentVars, Answer) :-
	% TODO: add check for is_monomial
	extract_vars(VarsPowers, CurrentVars, NewCurrentVars),
	variables_worker(RestOfMonomials, NewCurrentVars, Answer).
