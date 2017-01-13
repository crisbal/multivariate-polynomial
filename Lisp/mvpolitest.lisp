;;;; 807147 Nisoli Eric
;;;; No collaborators

;;;; mvpoli-test.lisp

;;;; Unit testing file for mvpoli library
;;;; Place in the same directory of mvpoli.lisp file,
;;;; then execute/load this file in order to test project required functionalities.

(defparameter *monomial-parsing-ok* 0)

(defparameter *monomial-parsing-fails* 0)

(defparameter *monomial-parsing-errors* 0)

(defparameter *polynomial-parsing-ok* 0)

(defparameter *polynomial-parsing-fails* 0)

(defparameter *polynomial-parsing-errors* 0)

(defparameter *utility-functions-ok* 0)

(defparameter *utility-functions-fails* 0)

(defparameter *utility-functions-errors* 0)

(defparameter *arithmetic-functions-ok* 0)

(defparameter *arithmetic-functions-fails* 0)

(defparameter *arithmetic-functions-errors* 0)

(defun update-test (&key category result)
    (cond ((eql category 'monomial-parsing)
	   	 (cond ((null result)
					(defparameter *monomial-parsing-fails*
					  		  (1+ *monomial-parsing-fails*)))
		       	       ((eql 'err result)
						(defparameter *monomial-parsing-errors*
						  		  (1+ *monomial-parsing-errors*)))
			       	       (T (defparameter *monomial-parsing-ok*
					    		    (1+ *monomial-parsing-ok*)))))

	          ((eql category 'polynomial-parsing)
		   	 (cond ((null result)
						(defparameter *polynomial-parsing-fails*
						  		  (1+ *polynomial-parsing-fails*)))
			       	       ((eql 'err result)
							(defparameter *polynomial-parsing-errors*
							  		  (1+ *polynomial-parsing-errors*)))
				       	       (T (defparameter *polynomial-parsing-ok*
						    		    (1+ *polynomial-parsing-ok*)))))

		          ((eql category 'utility-functions)
			   	 (cond ((null result)
							(defparameter *utility-functions-fails*
							  		  (1+ *utility-functions-fails*)))
				       	       ((eql 'err result)
								(defparameter *utility-functions-errors*
								  		  (1+ *utility-functions-errors*)))
					       	       (T (defparameter *utility-functions-ok*
							    		    (1+ *utility-functions-ok*)))))

			          ((eql category 'arithmetic-functions)
				   	 (cond ((null result)
								(defparameter *arithmetic-functions-fails*
								  		  (1+ *arithmetic-functions-fails*)))
					       	       ((eql 'err result)
									(defparameter *arithmetic-functions-errors*
									  		  (1+ *arithmetic-functions-errors*)))
						       	       (T (defparameter *arithmetic-functions-ok*
								    		    (1+ *arithmetic-functions-ok*)))))))

(defun test (name)
    (let ((*default-pathname-defaults* (make-pathname :defaults *load-truename*
						      						    :name NIL
												    						    :type NIL)))
          (load name)))

(defun test-function (&key result input1 input2 function category)
    (let* ((expected result)
	   	 (expression1 input1)
		 	 (expression2 input2)
			 	 (parsed (handler-case (if (null expression2)
							 				   (funcall function expression1)
											   				   (funcall function expression1 expression2))
					   		   (condition () 'err)))
				 	 (res (if (eql parsed 'err)
								  'err
								  		  (equal expected parsed)))
					 	 (update (update-test :category category
								      			      :result res)))
          (cond ((eql res 'err)
		 	   (if (null input2)
			     	       (format t "Failed with errors~%-- Expected: ~s~%-- Using inputs~%-- 1) ~s"
					       		       expected input1)
				       	       (format t "Failed with errors~%-- Expected: ~s~%-- Using inputs:~%-- 1) ~s~%-- 2) ~s"
						       		       expected input1 input2)))
			  ((null res)
			   	   (if (null input2)
				     	       (format t "Failed~%-- Expected: ~s~%-- Got: ~s~%-- Using inputs~%-- 1) ~s"
						       		       expected parsed input1)
					       	       (format t "Failed~%-- Expected: ~s~%-- Got: ~s~%-- Using inputs:~%-- 1) ~s~%-- 2) ~s"
							       		       expected parsed input1 input2)))
			  	  (T (format t "Ok")))))

(defun test-as-monomial ()
    (test-function :result '(M 3 5 ((V 2 X) (V 3 Y)))
		   		 :input1 '(* (* 3 (/ 3 3)) (expt x 2) (expt y 3))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-with-only-functor-symbol ()
    (test-function :result '(M 1 0 NIL)
		   		 :input1 '(*)
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-without-coefficient ()
    (test-function :result '(M 1 5 ((V 2 X) (V 3 Y)))
		   		 :input1 '(* (expt x 2) (expt y 3))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-expressed-by-only-a-number ()
    (test-function :result '(M 3 0 NIL)
		   		 :input1 '3
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-with-variable-expressed-only-by-a-symbol ()
    (test-function :result '(M 3 1 ((V 1 X)))
		   		 :input1 '(* 3 x)
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-with-variable-expressed-by-several-symbols ()
    (test-function :result '(M 3 2 ((V 2 TEST)))
		   		 :input1 '(* 3 (expt test 2))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-variables-reduction ()
    (test-function :result '(M 3 3 ((V 3 Y)))
		   		 :input1 '(* 3 (expt x 0) (expt y 3))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-monomial-variables-minimization ()
    (test-function :result '(M 3 5 ((V 5 X)))
		   		 :input1 '(* 3 (expt x 2) (expt x 3))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-monomial-sorting ()
    (test-function :result '(M 3 5 ((V 2 X) (V 3 Y)))
		   		 :input1 '(* 3 (expt y 3) (expt x 2))
				 		 :function #'as-monomial
						 		 :category 'monomial-parsing))

(defun test-as-polynomial ()
    (test-function :result '(POLY ((M 3 5 ((V 2 X) (V 3 Y)))))
		   		 :input1 '(+ (* 3 (expt y 3) (expt x 2)))
				 		 :function #'as-polynomial
						 		 :category 'polynomial-parsing))

(defun test-as-polynomial-of-monomial-expression ()
    (test-function :result '(POLY ((M 2 0 NIL)))
		   		 :input1 '(* 2)
				 		 :function #'as-polynomial
						 		 :category 'polynomial-parsing))

(defun test-as-polynomial-expressed-only-by-a-number ()
    (test-function :result '(POLY ((M 2 0 NIL)))
		   		 :input1 '2
				 		 :function #'as-polynomial
						 		 :category 'polynomial-parsing))

(defun test-as-polynomial-with-null-monomial ()
    (test-function :result '(POLY NIL)
		   		 :input1 '0
				 		 :function #'as-polynomial
						 		 :category 'polynomial-parsing))

(defun test-polynomial-sorting ()
    (test-function :result '(POLY ((M 1 1 ((V 1 X)))
				   				 (M -2 2 ((V 1 X) (V 1 Y)))
								 				 (M 1 2 ((V 2 X)))))
		   		 :input1 '(+ (* (expt x 2)) (* x) (* -2 y x))
				 		 :function #'as-polynomial
						 		 :category 'polynomial-parsing))

(defun test-varpowers ()
    (test-function :result '((V 1 X) (V 1 Y))
		   		 :input1 '(M 1 2 ((V 1 X) (V 1 Y)))
				 		 :function #'varpowers
						 		 :category 'utility-functions))

(defun test-varpowers-with-no-variables ()
    (test-function :result 'NIL
		   		 :input1 '(* 0 x)
				 		 :function #'varpowers
						 		 :category 'utility-functions))

(defun test-vars-of ()
    (test-function :result '(X Y)
		   		 :input1 '(* x y)
				 		 :function #'vars-of
						 		 :category 'utility-functions))

(defun test-vars-of-with-no-variables ()
    (test-function :result 'NIL
		   		 :input1 '(* 0 x)
				 		 :function #'vars-of
						 		 :category 'utility-functions))

(defun test-monomial-degree ()
    (test-function :result '3
		   		 :input1 '(* 3 x (expt y 2))
				 		 :function #'monomial-degree
						 		 :category 'utility-functions))

(defun test-null-monomial-degree ()
    (test-function :result '0
		   		 :input1 '(* 3)
				 		 :function #'monomial-degree
						 		 :category 'utility-functions))

(defun test-monomial-coefficient ()
    (test-function :result '1
		   		 :input1 '(* y z)
				 		 :function #'monomial-coefficient
						 		 :category 'utility-functions))

(defun test-null-monomial-coefficient ()
    (test-function :result '0
		   		 :input1 '(* 0 y z)
				 		 :function #'monomial-coefficient
						 		 :category 'utility-functions))

(defun test-coefficients ()
    (test-function :result '(-2 3 1)
		   		 :input1 '(+ (* 3 x) -2 (* y))
				 		 :function #'coefficients
						 		 :category 'utility-functions))

(defun test-coefficients-of-null-polynomial ()
    (test-function :result '(0)
		   		 :input1 '(+ (* -3 x) (* 3 x))
				 		 :function #'coefficients
						 		 :category 'utility-functions))

(defun test-variables ()
    (test-function :result '(x y z)
		   		 :input1 '(+ (* 3 x) -2 (* y z))
				 		 :function #'variables
						 		 :category 'utility-functions))

(defun test-monomials ()
    (test-function :result '((M 1 0 NIL)
			     			   (M 1 3 ((V 1 X) (V 1 Y) (V 1 Z)))
						   			   (M -1 3 ((V 1 X) (V 2 Y))))
		   		 :input1 '(+ (* x y z) 1 (* -1 x (expt y 2)))
				 		 :function #'monomials
						 		 :category 'utility-functions))

(defun test-maxdegree ()
    (test-function :result '3
		   		 :input1 '(+ 4 (* x y z) (* x y))
				 		 :function #'maxdegree
						 		 :category 'utility-functions))

(defun test-mindegree ()
    (test-function :result '0
		   		 :input1 '(+ 4 (* x y z) (* x y))
				 		 :function #'mindegree
						 		 :category 'utility-functions))

(defun test-polyplus ()
    (test-function :result '(POLY ((M 2 0 NIL)
				   				 (M 1 1 ((V 1 X)))
								 				 (M 2 3 ((V 2 X) (V 1 Y)))))
		   		 :input1 '(+ -1 (* x x y))
				 		 :input2 '(+ 3 (* x) (* x x y))
						 		 :function #'polyplus
								 		 :category 'arithmetic-functions))

(defun test-polyminus ()
    (test-function :result '(POLY ((M -4 0 NIL)
				   				 (M -1 1 ((V 1 X)))))
		   		 :input1 '(+ -1 (* x x y))
				 		 :input2 '(+ 3 (* x) (* x x y))
						 		 :function #'polyminus
								 		 :category 'arithmetic-functions))

(defun test-polyplus-0 ()
    (test-function :result '(POLY NIL)
		   		 :input1 '(+ (* -1 x x y))
				 		 :input2 '(+ (* x x y))
						 		 :function #'polyplus
								 		 :category 'arithmetic-functions))

(defun test-polyminus-0 ()
    (test-function :result '(POLY NIL)
		   		 :input1 '(+ -1 (* x x y))
				 		 :input2 '(+ -1 (* x x y))
						 		 :function #'polyminus
								 		 :category 'arithmetic-functions))

(defun test-polyplus-with-first-argument-0 ()
    (test-function :result '(POLY ((M 3 0 NIL)))
		   		 :input1 '0
				 		 :input2 '(+ 3)
						 		 :function #'polyplus
								 		 :category 'arithmetic-functions))

(defun test-polyplus-with-second-argument-0 ()
    (test-function :result '(POLY ((M 3 0 NIL)))
		   		 :input1 '(+ 3)
				 		 :input2 '(POLY NIL)
						 		 :function #'polyplus
								 		 :category 'arithmetic-functions))

(defun test-polyminus-with-first-argument-0 ()
    (test-function :result '(POLY ((M -3 0 NIL)))
		   		 :input1 '0
				 		 :input2 '(+ 3)
						 		 :function #'polyminus
								 		 :category 'arithmetic-functions))

(defun test-polyminus-with-second-argument-0 ()
    (test-function :result '(POLY ((M 3 0 NIL)))
		   		 :input1 '(+ 3)
				 		 :input2 '(POLY NIL)
						 		 :function #'polyminus
								 		 :category 'arithmetic-functions))

(defun test-polytimes ()
    (test-function :result '(POLY ((M 6 1 ((V 1 X)))
				   				 (M 2 3 ((V 2 X) (V 1 Y)))))
		   		 :input1 '(+ (* -2 x))
				 		 :input2 '(+ -3 (* -1 x y))
						 		 :function #'polytimes
								 		 :category 'arithmetic-functions))

(defun test-polytimes-minimization ()
    (test-function :result '(POLY ((M -1 0 NIL) (M 1 2 ((V 2 X)))))
		   		 :input1 '(+ (* x) 1)
				 		 :input2 '(+ (* x) -1)
						 		 :function #'polytimes
								 		 :category 'arithmetic-functions))

(defun test-polytimes-with-first-argument-0 ()
    (test-function :result '(POLY NIL)
		   		 :input1 '0
				 		 :input2 '(+ 3)
						 		 :function #'polytimes
								 		 :category 'arithmetic-functions))

(defun test-polytimes-with-second-argument-0 ()
    (test-function :result '(POLY NIL)
		   		 :input1 '(+ 3)
				 		 :input2 '(POLY NIL)
						 		 :function #'polytimes
								 		 :category 'arithmetic-functions))

(defun test-polytimes-with-first-argument-1 ()
    (test-function :result '(POLY ((M 3 0 NIL)
				   				 (M 1 1 ((V 1 X)))))
		   		 :input1 '1
				 		 :input2 '(+ (* x) 3)
						 		 :function #'polytimes))

(defun test-polytimes-with-second-argument-1 ()
    (test-function :result '(POLY ((M 3 0 NIL)
				   				 (M 1 1 ((V 1 X)))))
		   		 :input1 '(+ (* x) 3)
				 		 :input2 '1
						 		 :function #'polytimes
								 		 :category 'arithmetic-functions))

(defun test-polyval ()
    (test-function :result '-80
		   		 :input1 '(+ (* (* (+ -3 2) (+ 3 2)) x (expt y 3)) (* 0 z))
				 		 :input2 '(2 2)
						 		 :function #'polyval
								 		 :category 'arithmetic-functions))

(defun test-polyval-minimization ()
    (test-function :result '0
		   		 :input1 '(+ (* x) (* -1 x))
				 		 :input2 '(1 2)
						 		 :function #'polyval
								 		 :category 'arithmetic-functions))

(defun test-polyval-with-more-variable-values ()
    (test-function :result '16
		   		 :input1 '(+ (* x (expt y 3)) (* 0 z))
				 		 :input2 '(2 2 3)
						 		 :function #'polyval
								 		 :category 'arithmetic-functions))

(test "mvpoli")

(format t "~&~%-- Begin tests --~%~%-- Monomial parsing tests--")

(format t "~&Testing monomial parsing...")
(test-as-monomial)

(format t "~&Testing monomial parsing represented by only functor symbol...")
(test-as-monomial-with-only-functor-symbol)

(format t "~&Testing monomial parsing without coefficient...")
(test-as-monomial-without-coefficient)

(format t "~&Testing monomial parsing represented only by a number...")
(test-as-monomial-expressed-by-only-a-number)

(format t "~&Testing monomial parsing with a variable expressed only by a symbol...")
(test-as-monomial-with-variable-expressed-only-by-a-symbol)

(format t "~&Testing monomial parsing with a variable expressed by several symbols...")
(test-as-monomial-with-variable-expressed-by-several-symbols)

(format t "~&Testing monomial parsing with 0 as variable power...")
(test-as-monomial-variables-reduction)

(format t "~&Testing monomial parsing using variables with same symbol...")
(test-as-monomial-variables-minimization)

(format t "~&Testing monomial sorting...")
(test-monomial-sorting)

(format t "~&~%-- Polynomial parsing tests --")

(format t "~&Testing polynomial parsing...")
(test-as-polynomial)

(format t "~&Testing polynomial parsing of monomial expression...")
(test-as-polynomial-of-monomial-expression)

(format t "~&Testing polynomial parsing of number expression...")
(test-as-polynomial-expressed-only-by-a-number)

(format t "~&Testing null polynomial parsing...")
(test-as-polynomial-with-null-monomial)

(format t "~&Testing polynomial sorting...")
(test-polynomial-sorting)

(format t "~&~%-- Utility functions tests --")

(format t "~&Testing varpowers...")
(test-varpowers)

(format t "~&Testing varpowers with no variables...")
(test-varpowers-with-no-variables)

(format t "~&Testing vars-of...")
(test-vars-of)

(format t "~&Testing vars-of with no variables...")
(test-vars-of-with-no-variables)

(format t "~&Testing monomial-degree...")
(test-monomial-degree)

(format t "~&Testing null monomial-degree...")
(test-null-monomial-degree)

(format t "~&Testing monomial-coefficient...")
(test-monomial-coefficient)

(format t "~&Testing null monomial-coefficient...")
(test-null-monomial-coefficient)

(format t "~&Testing coefficients...")
(test-coefficients)

(format t "~&Testing coefficients of null polynomial...")
(test-coefficients-of-null-polynomial)

(format t "~&Testing variables...")
(test-variables)

(format t "~&Testing monomials...")
(test-monomials)

(format t "~&Testing maxdegree...")
(test-maxdegree)

(format t "~&Testing mindegree...")
(test-mindegree)

(format t "~&~%-- Arithmetic functions tests --")

(format t "~&Testing polyplus...")
(test-polyplus)

(format t "~&Testing polyminus...")
(test-polyminus)

(format t "~&Testing polyplus with 0 as result...")
(test-polyplus-0)

(format t "~&Testing polyminus with 0 as result...")
(test-polyminus-0)

(format t "~&Testing polyplus using null polynomial as first value...")
(test-polyplus-with-first-argument-0)

(format t "~&Testing polyplus using null polynomial as second value...")
(test-polyplus-with-second-argument-0)

(format t "~&Testing polyminus using null polynomial as first value...")
(test-polyminus-with-first-argument-0)

(format t "~&Testing polyminus using null polynomial as second value...")
(test-polyminus-with-second-argument-0)

(format t "~&Testing polytimes...")
(test-polytimes)

(format t "~&Testing polytimes minimimization...")
(test-polytimes-minimization)

(format t "~&Testing polytimes using null polynomial as first value...")
(test-polytimes-with-first-argument-0)

(format t "~&Testing polytimes using null polynomial as secondfirst value...")
(test-polytimes-with-second-argument-0)

(format t "~&Testing polytimes using neutral polynomial as first value...")
(test-polytimes-with-first-argument-1)

(format t "~&Testing polytimes using neutral polynomial as secondfirst value...")
(test-polytimes-with-second-argument-1)

(format t "~&Testing polyval...")
(test-polyval)

(format t "~&Testing polyval minimization...")
(test-polyval-minimization)

(format t "~&Testing polyval with more variable values then variables...")
(test-polyval-with-more-variable-values)

(format t
		"~&~%Tests result, passed: ~d, failed: ~d, errors: ~d"
			(+ *monomial-parsing-ok* *polynomial-parsing-ok*
			   	   *utility-functions-ok* *arithmetic-functions-ok*)
				(+ *monomial-parsing-fails* *polynomial-parsing-fails*
				   	   *utility-functions-fails* *arithmetic-functions-fails*)
					(+ *monomial-parsing-errors* *polynomial-parsing-errors*
					   	   *utility-functions-errors* *arithmetic-functions-errors*))

(format t
		"~&Monomial parsing tests, passed: ~d, failed: ~d, errors: ~d"
			*monomial-parsing-ok*
				*monomial-parsing-fails*
					*monomial-parsing-errors*)

(format t
		"~&Polynomial parsing tests, passed: ~d, failed: ~d, errors: ~d"
			*polynomial-parsing-ok*
				*polynomial-parsing-fails*
					*polynomial-parsing-errors*)

(format t
		"~&Utility functions tests, passed: ~d, failed: ~d, errors: ~d"
			*utility-functions-ok*
				*utility-functions-fails*
					*utility-functions-errors*)

(format t
		"~&Arithmetic functions tests, passed: ~d, failed: ~d, errors: ~d"
			*arithmetic-functions-ok*
				*arithmetic-functions-fails*
					*arithmetic-functions-errors*)

;;;; end of file -- mvpoli-test.lisp
