%% is_monomial/1
% we add some checks to make sure that is_monomial makes some sense, especially
% we have to check if the totaldegree is correct 
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	% handle_zero_coefficient(C, VPs, VPs), %TODO: is needed?
	compute_total_degree_for_vars_powers(VPs, TD), %TODO: is needed?
	is_list(VPs),
	foreach(member(V, VPs), is_varpower(V)),
	!.
