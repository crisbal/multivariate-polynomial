%% is_varpower/1
% FIXME? do we really have to check if the exponent is negative??
is_varpower(v(Exponent, Variable)) :-
	integer(Exponent),
	Exponent >= 0, %TODO: why?
	atomic(Variable).

