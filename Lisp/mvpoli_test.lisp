(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "clunit")

(clunit:defsuite mvpoli ())

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

(defun run-tests()
	(clunit:run-suite 'mvpoli :report-progress T))

(run-tests)
