varpowersval([], _, _, 1) :- !.
varpowersval([v(Exponent, Base) | OtherVPs], VarSymbols, VarValues, Value) :-
	find_var_value(Base, VarSymbols, VarValues, BaseValue),
	CurrentValue is BaseValue^Exponent,
	varpowersval(OtherVPs, VarSymbols, VarValues, OtherValue),
	Value is CurrentValue*OtherValue.
