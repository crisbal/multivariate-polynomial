%% is_varpower/1
% as defined in the requirements of the project
is_varpower(v(Exponent, Variable)) :-
	integer(Exponent),
	Exponent >= 0,
	atomic(Variable).

