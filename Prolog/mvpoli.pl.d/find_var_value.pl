%% find_var_value/4
% TODO: investigate better solution with dictionaries
% given a symbol, a list of symbols and a list of the values associated with 
% the symbols the values associated with the imput will be returned
find_var_value(Symbol, [Symbol | _], [SymbolValue | _], SymbolValue) :- !.
find_var_value(Symbol, [NotMySymbol | OtherSymbols], 
	[_ | OtherSymbolsValue], FoundSymbolValue) :-
	Symbol \= NotMySymbol,
	find_var_value(Symbol, OtherSymbols, OtherSymbolsValue, FoundSymbolValue),
	!.
