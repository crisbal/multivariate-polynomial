%% is_var_power/1
% FIXME? do we really have to check if the exponent is negative??
is_var_power(v(Exponent, Variable)) :-
	integer(Exponent),
	Exponent >= 0, %TODO: why?
	atomic(Variable).
