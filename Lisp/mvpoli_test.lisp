(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "clunit")

(clunit:defsuite mvpoli ())

"Tests for is-varpower"
(clunit:defsuite is-varpower (mvpoli))

(clunit:deftest test-is-varpower-1 (is-varpower)
  (clunit:assert-true (is-varpower '(v 1 x))))

(clunit:deftest test-is-varpower-2 (is-varpower)
  (clunit:assert-false (is-varpower '(v 1 x e))))

(clunit:deftest test-is-varpower-3 (is-varpower)
  (clunit:assert-false (is-varpower '(v e x))))

(clunit:deftest test-is-varpower-4 (is-varpower)
  (clunit:assert-false (is-varpower '(v 1 1))))

"Tests for eval-as-number"
(clunit:defsuite eval-as-number (mvpoli))

(clunit:deftest test-eval-as-number-1 (eval-as-number)
  (clunit:assert-true  (= 12 (eval-as-number '(* 3 4)))))

(clunit:deftest test-eval-as-number-2 (eval-as-number)
  (clunit:assert-true  (= 48 (eval-as-number '(* (* 3 4) (+ 2 2))))))

"Tests for exptp"
(clunit:defsuite exptp (mvpoli))

(clunit:deftest test-exptp-1 (exptp)
  (clunit:assert-true  (exptp '(expt x 4))))

(clunit:deftest test-exptp-2 (exptp)
  (clunit:assert-true  (exptp '(expt y 1))))

"Tests for expression-variable-p"
(clunit:defsuite expression-variable-p (mvpoli))

(clunit:deftest test-expression-variable-p-1 (expression-variable-p)
  (clunit:assert-true  (expression-variable-p '(expt x 4))))

(clunit:deftest test-expression-variable-p-2 (expression-variable-p)
  (clunit:assert-true  (expression-variable-p 't)))

"Tests for monomial-expression-component-p"
(clunit:defsuite monomial-expression-component-p (mvpoli))

(clunit:deftest test-monomial-expression-component-p-1 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p '(expt x 4))))

(clunit:deftest test-monomial-expression-component-p-2 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p 't)))

(clunit:deftest test-monomial-expression-component-p-3 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p 4)))

(clunit:deftest test-monomial-expression-component-p-4 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p '4)))

"Tests foreval-as-number"
(clunit:defsuite eval-as-number (mvpoli))
(clunit:deftest test-eval-as-number-1 (eval-as-number) (clunit:assert-true T))

"Tests foris-varpower"
(clunit:defsuite is-varpower (mvpoli))
(clunit:deftest test-is-varpower-1 (is-varpower) (clunit:assert-true T))

"Tests forvarpower-power"
(clunit:defsuite varpower-power (mvpoli))
(clunit:deftest test-varpower-power-1 (varpower-power) (clunit:assert-true T))

"Tests forvarpower-symbol"
(clunit:defsuite varpower-symbol (mvpoli))
(clunit:deftest test-varpower-symbol-1 (varpower-symbol) (clunit:assert-true T))

"Tests foris-monomial"
(clunit:defsuite is-monomial (mvpoli))
(clunit:deftest test-is-monomial-1 (is-monomial) (clunit:assert-true T))

"Tests formonomial-varpowers"
(clunit:defsuite monomial-varpowers (mvpoli))
(clunit:deftest test-monomial-varpowers-1 (monomial-varpowers) (clunit:assert-true T))

"Tests formonomial-degree"
(clunit:defsuite monomial-degree (mvpoli))
(clunit:deftest test-monomial-degree-1 (monomial-degree) (clunit:assert-true T))

"Tests formonomial-coefficient"
(clunit:defsuite monomial-coefficient (mvpoli))
(clunit:deftest test-monomial-coefficient-1 (monomial-coefficient) (clunit:assert-true T))

"Tests forcompare-varpowers"
(clunit:defsuite compare-varpowers (mvpoli))
(clunit:deftest test-compare-varpowers-1 (compare-varpowers) (clunit:assert-true T))

"Tests forlesser-varpower"
(clunit:defsuite lesser-varpower (mvpoli))
(clunit:deftest test-lesser-varpower-1 (lesser-varpower) (clunit:assert-true T))

"Tests forcompare-monomials"
(clunit:defsuite compare-monomials (mvpoli))
(clunit:deftest test-compare-monomials-1 (compare-monomials) (clunit:assert-true T))

"Tests forexptp"
(clunit:defsuite exptp (mvpoli))
(clunit:deftest test-exptp-1 (exptp) (clunit:assert-true T))

"Tests forexpression-variable-p"
(clunit:defsuite expression-variable-p (mvpoli))
(clunit:deftest test-expression-variable-p-1 (expression-variable-p) (clunit:assert-true T))

"Tests formonomial-expression-component-p"
(clunit:defsuite monomial-expression-component-p (mvpoli))
(clunit:deftest test-monomial-expression-component-p-1 (monomial-expression-component-p) (clunit:assert-true T))

"Tests formonomial-expression-p"
(clunit:defsuite monomial-expression-p (mvpoli))
(clunit:deftest test-monomial-expression-p-1 (monomial-expression-p) (clunit:assert-true T))

"Tests forcoefficients"
(clunit:defsuite coefficients (mvpoli))
(clunit:deftest test-coefficients-1 (coefficients) (clunit:assert-true T))

"Tests fortotal-coefficient"
(clunit:defsuite total-coefficient (mvpoli))
(clunit:deftest test-total-coefficient-1 (total-coefficient) (clunit:assert-true T))

"Tests forexpression-variables"
(clunit:defsuite expression-variables (mvpoli))
(clunit:deftest test-expression-variables-1 (expression-variables) (clunit:assert-true T))

"Tests forexpression-variable-to-varpower"
(clunit:defsuite expression-variable-to-varpower (mvpoli))
(clunit:deftest test-expression-variable-to-varpower-1 (expression-variable-to-varpower) (clunit:assert-true T))

"Tests forvarpowers-from-expression"
(clunit:defsuite varpowers-from-expression (mvpoli))
(clunit:deftest test-varpowers-from-expression-1 (varpowers-from-expression) (clunit:assert-true T))

"Tests forsort-varpowers"
(clunit:defsuite sort-varpowers (mvpoli))
(clunit:deftest test-sort-varpowers-1 (sort-varpowers) (clunit:assert-true T))

"Tests fortotal-degree-varpowers"
(clunit:defsuite total-degree-varpowers (mvpoli))
(clunit:deftest test-total-degree-varpowers-1 (total-degree-varpowers) (clunit:assert-true T))

"Tests forbuild-monomial-object"
(clunit:defsuite build-monomial-object (mvpoli))
(clunit:deftest test-build-monomial-object-1 (build-monomial-object) (clunit:assert-true T))

"Tests forcompress-varpowers-reducer"
(clunit:defsuite compress-varpowers-reducer (mvpoli))
(clunit:deftest test-compress-varpowers-reducer-1 (compress-varpowers-reducer) (clunit:assert-true T))

"Tests forcompress-varpowers"
(clunit:defsuite compress-varpowers (mvpoli))
(clunit:deftest test-compress-varpowers-1 (compress-varpowers) (clunit:assert-true T))

"Tests forparse-monomial-expression"
(clunit:defsuite parse-monomial-expression (mvpoli))
(clunit:deftest test-parse-monomial-expression-1 (parse-monomial-expression) (clunit:assert-true T))

"Tests foras-monomial"
(clunit:defsuite as-monomial (mvpoli))
(clunit:deftest test-as-monomial-1 (as-monomial) (clunit:assert-true T))

"Tests forpolynomial-expression-p"
(clunit:defsuite polynomial-expression-p (mvpoli))
(clunit:deftest test-polynomial-expression-p-1 (polynomial-expression-p) (clunit:assert-true T))

"Tests forsort-monomials"
(clunit:defsuite sort-monomials (mvpoli))
(clunit:deftest test-sort-monomials-1 (sort-monomials) (clunit:assert-true T))

"Tests forcompress-monomials-reducer"
(clunit:defsuite compress-monomials-reducer (mvpoli))
(clunit:deftest test-compress-monomials-reducer-1 (compress-monomials-reducer) (clunit:assert-true T))

"Tests forcompress-monomials"
(clunit:defsuite compress-monomials (mvpoli))
(clunit:deftest test-compress-monomials-1 (compress-monomials) (clunit:assert-true T))

"Tests forparse-polynomial-expression"
(clunit:defsuite parse-polynomial-expression (mvpoli))
(clunit:deftest test-parse-polynomial-expression-1 (parse-polynomial-expression) (clunit:assert-true T))

"Tests foras-polynomial"
(clunit:defsuite as-polynomial (mvpoli))
(clunit:deftest test-as-polynomial-1 (as-polynomial) (clunit:assert-true T))

(defun run-tests()
	(clunit:run-suite 'mvpoli :report-progress T))

(run-tests)
