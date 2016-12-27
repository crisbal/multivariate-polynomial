(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(load "mvpoli.lisp")
(quicklisp:quickload "clunit")

(clunit:defsuite mvpoli ())

(clunit:defsuite builder (mvpoli))
"Tests for object builders"

(clunit:deftest test-build-varpower-object (builder)
  (clunit:assert-equal (build-varpower-object 'x 3) '(V 3 x)))

(clunit:deftest test-build-monomial-object (builder)
  (clunit:assert-equal (build-monomial-object 1 5 '((V 5 x))) '(M 1 5 ((V 5 x)))))

(clunit:deftest test-build-polynomial-object (builder)
  (clunit:assert-equal (build-polynomial-object '()) '(P ())))

(clunit:defsuite varpower (mvpoli))
"Tests for varpower-related functions"

(clunit:defsuite is-varpower (varpower))
"Tests for is-varpower"

(clunit:deftest test-is-varpower-1 (is-varpower)
  (clunit:assert-true (is-varpower '(v 1 x))))

(clunit:deftest test-is-varpower-2 (is-varpower)
  (clunit:assert-false (is-varpower '(v 1 x e))))

(clunit:deftest test-is-varpower-3 (is-varpower)
  (clunit:assert-false (is-varpower '(v e x))))

(clunit:deftest test-is-varpower-4 (is-varpower)
  (clunit:assert-false (is-varpower '(v 1 1))))

(clunit:defsuite varpower-power (varpower))
"Tests for varpower-power"

(clunit:deftest test-varpower-power-1 (varpower-power)
  (clunit:assert-equal (varpower-power '(V 1 x)) 1))

(clunit:defsuite varpower-symbol (varpower))
"Tests for varpower-symbol"

(clunit:deftest test-varpower-symbol-1 (varpower-symbol)
  (clunit:assert-equal (varpower-symbol '(V 1 x)) 'x))

(clunit:defsuite helper (mvpoli))
"Tests for helper functions"

(clunit:defsuite eval-as-number (helper))
"Tests for eval-as-number"

(clunit:deftest test-eval-as-number-1 (eval-as-number)
  (clunit:assert-equal 12 (eval-as-number '(* 3 4))))

(clunit:deftest test-eval-as-number-2 (eval-as-number)
  (clunit:assert-equal 48 (eval-as-number '(* (* 3 4) (+ 2 2)))))

(clunit:deftest test-eval-as-number-3 (eval-as-number)
  (clunit:assert-equal (cos 3) (eval-as-number '(cos 3))))

(clunit:defsuite expression (mvpoli))
"Test for expression parsing"

(clunit:defsuite exptp (expression))
"Tests for the exptp function"

(clunit:deftest test-exptp-1 (exptp)
  (clunit:assert-true  (exptp '(expt x 4))))

(clunit:deftest test-exptp-2 (exptp)
  (clunit:assert-true  (exptp '(expt y 1))))

"Tests for expression-variable-p"
(clunit:defsuite expression-variable-p (expression))

(clunit:deftest test-expression-variable-p-1 (expression-variable-p)
  (clunit:assert-true  (expression-variable-p '(expt x 4))))

(clunit:deftest test-expression-variable-p-2 (expression-variable-p)
  (clunit:assert-true  (expression-variable-p 't)))

(clunit:deftest test-expression-variable-p-3 (expression-variable-p)
  (clunit:assert-false  (expression-variable-p 4)))

"Tests for monomial-expression-component-p"
(clunit:defsuite monomial-expression-component-p (expression))

(clunit:deftest test-monomial-expression-component-p-1 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p '(expt x 4))))

(clunit:deftest test-monomial-expression-component-p-2 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p 't)))

(clunit:deftest test-monomial-expression-component-p-3 (monomial-expression-component-p)
  (clunit:assert-true  (monomial-expression-component-p 4)))

"Tests for monomial-related functions"
(clunit:defsuite monomial (mvpoli))

(defparameter expression '(* 3 x 2 y x (expt z 3) (expt q 0)))
(defparameter example-monomial (as-monomial expression))
"Tests for is-monomial"
(clunit:defsuite is-monomial (monomial))

(clunit:deftest test-is-monomial-1 (is-monomial)
  (clunit:assert-true (is-monomial example-monomial)))

(clunit:deftest test-is-monomial-2 (is-monomial)
  (clunit:assert-false (is-monomial '(r e 2))))

(clunit:deftest test-is-monomial-3 (is-monomial)
  (clunit:assert-false (is-monomial '(M 6 6 ((Q 2 X) (V 1 Y) (V 3 Z))))))

(clunit:deftest test-is-monomial-4 (is-monomial)
  (clunit:assert-true (is-monomial '(M 0 0 ()))))


"Tests for monomial-varpowers"
(clunit:defsuite monomial-varpowers (monomial))

(clunit:deftest test-monomial-varpowers-1 (monomial-varpowers)
  (clunit:assert-equal
   (monomial-varpowers example-monomial)
   '((V 2 X) (V 1 Y) (V 3 Z))))

"Tests for monomial-degree"
(clunit:defsuite monomial-degree (monomial))

(clunit:deftest test-monomial-degree-1 (monomial-degree)
  (clunit:assert-equal (monomial-degree example-monomial) 6))

"Tests for monomial-coefficient"
(clunit:defsuite monomial-coefficient (monomial))

(clunit:deftest test-monomial-coefficient-1 (monomial-coefficient)
  (clunit:assert-equal (monomial-coefficient example-monomial) 6))

"Tests for compare-varpowers"
(clunit:defsuite compare-varpowers (varpower))

(clunit:deftest test-compare-varpowers-1 (compare-varpowers)
  (clunit:assert-true (compare-varpowers '(V 2 X) '(V 3 Y))))

(clunit:deftest test-compare-varpowers-2 (compare-varpowers)
  (clunit:assert-false (compare-varpowers '(V 2 Y) '(V 3 X))))

"Tests for lesser-varpower"
(clunit:defsuite lesser-varpower (varpower))

(clunit:deftest test-lesser-varpower-1 (lesser-varpower)
  (warn "IMPLEMENT TEST FOR LESSER-VARPOWER OR DELETE IT")
  (clunit:assert-true T))

"Tests for compare-monomials"
(clunit:defsuite compare-monomials (monomial))

(clunit:deftest test-compare-monomials-1 (compare-monomials)
  (warn "IMPLEMENT TEST FOR COMPARE-MONOMIALS OR DELETE IT")
  (clunit:assert-true T))

(clunit:defsuite monomial-expression-p (expression))
"Tests for monomial-expression-p"

(clunit:deftest test-monomial-expression-p-1 (monomial-expression-p)
  (clunit:assert-true (monomial-expression-p '(* 1 2 x y))))

(clunit:deftest test-monomial-expression-p-2 (monomial-expression-p)
  (clunit:assert-false (monomial-expression-p '(+ 1 2 x y))))

"Tests for coefficients"
(clunit:defsuite expression-coefficient (expression))

(clunit:defsuite coefficients (expression-coefficient))
(clunit:deftest test-coefficients-1 (coefficients)
  (clunit:assert-equal (coefficients '(2 3 x y (exptp x 3) 4)) '(2 3 4)))

"Tests for total-coefficient"
(clunit:defsuite total-coefficient (expression-coefficient))

(clunit:deftest test-total-coefficient-1 (total-coefficient)
  (clunit:assert-equal (total-coefficient '(x y 3 z 4)) 12))

(clunit:deftest test-total-coefficient-2 (total-coefficient)
  (clunit:assert-equal (total-coefficient '(x y z )) 1))

"Tests for expression-variables"
(clunit:defsuite expression-variables (mvpoli))

(clunit:deftest test-expression-variables-1 (expression-variables)
  (clunit:assert-true T))

"Tests for expression-variable-to-varpower"
(clunit:defsuite expression-variable-to-varpower (mvpoli))

(clunit:deftest test-expression-variable-to-varpower-1 (expression-variable-to-varpower)
  (clunit:assert-true T))

"Tests for varpowers-from-expression"
(clunit:defsuite varpowers-from-expression (mvpoli))

(clunit:deftest test-varpowers-from-expression-1 (varpowers-from-expression)
  (clunit:assert-true T))

"Tests for sort-varpowers"
(clunit:defsuite sort-varpowers (mvpoli))

(clunit:deftest test-sort-varpowers-1 (sort-varpowers)
  (clunit:assert-true T))

"Tests for total-degree-varpowers"
(clunit:defsuite total-degree-varpowers (mvpoli))

(clunit:deftest test-total-degree-varpowers-1 (total-degree-varpowers)
  (clunit:assert-true T))

"Tests for build-monomial-object"
(clunit:defsuite build-monomial-object (mvpoli))

(clunit:deftest test-build-monomial-object-1 (build-monomial-object)
  (clunit:assert-true T))

"Tests for compress-varpowers-reducer"
(clunit:defsuite compress-varpowers-reducer (mvpoli))

(clunit:deftest test-compress-varpowers-reducer-1 (compress-varpowers-reducer)
  (clunit:assert-true T))

"Tests for compress-varpowers"
(clunit:defsuite compress-varpowers (mvpoli))

(clunit:deftest test-compress-varpowers-1 (compress-varpowers)
  (clunit:assert-true T))

"Tests for parse-monomial-expression"
(clunit:defsuite parse-monomial-expression (mvpoli))

(clunit:deftest test-parse-monomial-expression-1 (parse-monomial-expression)
  (clunit:assert-true T))

"Tests for as-monomial"
(clunit:defsuite as-monomial (mvpoli))

(clunit:deftest test-as-monomial-1 (as-monomial)
  (clunit:assert-true T))

"Tests for polynomial-expression-p"
(clunit:defsuite polynomial-expression-p (mvpoli))

(clunit:deftest test-polynomial-expression-p-1 (polynomial-expression-p)
  (clunit:assert-true T))

"Tests for sort-monomials"
(clunit:defsuite sort-monomials (mvpoli))

(clunit:deftest test-sort-monomials-1 (sort-monomials)
  (clunit:assert-true T))

"Tests for compress-monomials-reducer"
(clunit:defsuite compress-monomials-reducer (mvpoli))

(clunit:deftest test-compress-monomials-reducer-1 (compress-monomials-reducer)
  (clunit:assert-true T))

"Tests for compress-monomials"
(clunit:defsuite compress-monomials (mvpoli))

(clunit:deftest test-compress-monomials-1 (compress-monomials)
  (clunit:assert-true T))

"Tests for parse-polynomial-expression"
(clunit:defsuite parse-polynomial-expression (mvpoli))

(clunit:deftest test-parse-polynomial-expression-1 (parse-polynomial-expression)
  (clunit:assert-true T))

"Tests for as-polynomial"
(clunit:defsuite as-polynomial (mvpoli))

(clunit:deftest test-as-polynomial-1 (as-polynomial)
  (clunit:assert-equal (as-polynomial '(+ (* -1 x) (* x w))) '(P ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 W) (V 1 X))))) ))


"Tests for is-polynomial"
(clunit:defsuite is-polynomial (mvpoli))

(clunit:deftest test-is-polynomial-1 (is-polynomial)
  (clunit:assert-true (is-polynomial '(P ((M 1 2 ((V 1 W) (V 1 X))) (M -1 1 ((V 1 X))))))))

(clunit:deftest test-is-polynomial-2 (is-polynomial)
  (clunit:assert-false (is-polynomial '(Q ((M 1 2 ((V 1 W) (V 1 X))) (M -1 1 ((V 1 X))))))))

(clunit:deftest test-is-polynomial-3 (is-polynomial)
  (clunit:assert-false (is-polynomial '(P ((Z 1 2 ((V 1 W) (V 1 X))) (M -1 1 ((V 1 X))))))))


(defun run-tests()
	(clunit:run-suite 'mvpoli :report-progress T))

(run-tests)
