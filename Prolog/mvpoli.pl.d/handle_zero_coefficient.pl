%% handle_zero_coefficient/3
% if coefficient is zero "returns" an empty list, otherwise the passed list
handle_zero_coefficient(0, _, []) :- !.
handle_zero_coefficient(_, VarsPowers, VarsPowers) :- !.

