%% as_var_power/2
% this will parse a variable or variable^exponent expression and produce
% the correct representation v(exponent, variable)
as_var_power(Variable, v(1, Variable)) :-
    is_varpower(v(1, Variable)),
    !.
as_var_power(Variable^Exponent, v(Exponent, Variable)) :-
	is_varpower(v(Exponent, Variable)),
	!.
/*as_var_power(Base^Other, v(NewExponent, BaseVariable)) :-
	integer(Exponent),
	as_var_power(Other, v(OtherExponent, BaseVariable)),
	NewExponent is Exponent*OtherExponent,
	!.
*/
