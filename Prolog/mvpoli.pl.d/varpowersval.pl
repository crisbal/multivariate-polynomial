%% varpowersval/4
% calculate the value of a VPs list. 
% first param is a list of VPs, second is a list of symbols, 3rd a list of
% the values associated with the symbols and 4th is the values calculated
varpowersval([], _, _, 1) :- !.
varpowersval([v(Exponent, Base) | OtherVPs], VarSymbols, VarValues, Value) :-
	find_var_value(Base, VarSymbols, VarValues, BaseValue),
	CurrentValue is BaseValue^Exponent,
	varpowersval(OtherVPs, VarSymbols, VarValues, OtherValue),
	Value is CurrentValue*OtherValue.
