%% compare_vars_powers/3
% this delta predicate is used by predsort/3 to sort vars powers
% this is really really useful since it takes away the need to write a
% sorting/looping algorithm and let us focus on the logic
% we don't provide an equality delta predicate (= is always false) since we
% cover all the cases with < and >. This is so we keep duplicates, which are
% usually deleted by predsort. compress_sorted_vps/3 will take care of the rest
compare_vars_powers(<, v(_E1, V1), v(_E2, V2)) :-
	V1 @=< V2, %=< so we keep duplicates of the same variable
	!.
compare_vars_powers(>, v(_E1, V1), v(_E2, V2)) :-
	V1 @>= V2, %>= so we keep duplicates of the same variable
	!.

