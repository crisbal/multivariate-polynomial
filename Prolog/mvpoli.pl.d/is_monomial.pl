%% is_monomial/1
is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD>=0, %TODO: why?
	compute_total_degree_for_vars_powers(VPs, TD), %TODO: is needed?
	is_list(VPs),
	predsort(compare_vars_powers, VPs, VPs), %TODO: is needed?
	compress_sorted_vps(VPs, VPs), %TODO: is needed
	!. %TODO: is needed?

