% TODO: disallow two way
% TODO: investigate better solution with dictionaries
find_var_value(Symbol, [Symbol | _], [SymbolValue | _], SymbolValue) :- !.
find_var_value(Symbol, [NotMySymbol | OtherSymbols], [_| OtherSymbolsValue], FoundSymbolValue) :-
	Symbol \= NotMySymbol,
	find_var_value(Symbol, OtherSymbols, OtherSymbolsValue, FoundSymbolValue),
	!.

