PROLOG = swipl
FILENAME ?= mvpoli

.PHONY: all
all: build repl

.PHONY: test
test: buildtest
	@ echo "+++ Run tests and exit +++"
	$(PROLOG) -s $(FILENAME) -g $(FILENAME)_test -t halt

.PHONY: repl
repl:
	$(PROLOG) -s $(FILENAME)

.PHONY: clean
clean:
	rm mvpoli.pl

.PHONY: build
build:
	@ echo -n																									>	 mvpoli.pl
	@ echo '%%%% XXXNUMBER XXXSURNAME XXXNAME' 												>> mvpoli.pl
	@ echo '%%%% XXXCO_NUMBER XXXCO_SURNAME XXXCO_NAME' 								>> mvpoli.pl
	@ echo 																										>> mvpoli.pl
	@ cat mvpoli.pl.d/is_var_power.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/as_var_power.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/is_monomial.pl 													>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/as_monomial.pl 													>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/is_polynomial.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/as_polynomial.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/polyval.pl 															>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/monoval.pl 															>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/varpowersval.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/pprint_polynomial.pl 										>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/pprint_monomial.pl 											>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/pprint_vars_powers.pl 									>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/coefficients.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/maxdegree.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/mindegree.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/variables.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/monomials.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/polyplus.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/polyminus.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/polytimes.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ echo 																										>> mvpoli.pl
	@ echo '%%% "helper"/not core rules'											>> mvpoli.pl
	@ echo																										>> mvpoli.pl
	@ cat mvpoli.pl.d/find_var_value.pl 											>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/extract_vars.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/to_polynomial.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl																	>> mvpoli.pl
	@ cat mvpoli.pl.d/monotimespoly.pl 												>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/monotimes.pl 														>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/monomials_times_minus_one.pl 						>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/compress_sorted_vps.pl 									>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/compress_sorted_monomials.pl 						>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/compute_total_degree_for_vars_powers.pl >> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/handle_zero_coefficient.pl 							>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/compare_vars_powers.pl 									>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/compare_monomials.pl 										>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl
	@ cat mvpoli.pl.d/is_vp_less_by_order.pl 									>> mvpoli.pl
	@ /bin/echo -e "\n"																				>> mvpoli.pl

buildtest: build
	@ cat mvpoli.pl.d/mvpoli_test.pl													>> mvpoli.pl
