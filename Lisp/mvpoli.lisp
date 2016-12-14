(defun eval-as-number(expression)
  "Eval EXPRESSION and return its numeric value"  
  (let ((result (handler-case (eval expression)
		  (error () nil)
		  (warning () nil))))
    (if (numberp result) result nil)))

(defun varpower-power(varpower)
  (when (and (listp varpower)
	     (numberp (second varpower)))
    (second varpower)))

(defun varpower-symbol(varpower)
  (when (and (listp varpower)
	     (symbolp (third varpower)))
    (third varpower)))

(defun is-varpower(varpower)
  (and (listp varpower)
       (eq 'v (first varpower))
       (let ((p (varpower-power varpower))
	     (s (varpower-symbol varpower)))
	 (and (integerp p)
	      (symbolp s)))))

(defun exptp(something)
  "T is returned when SOMETHING is a list in the form '(expt symbol number)"
  (and (listp something)
       (equal (first something) 'expt)
       (symbolp (second something))
       (numberp (third something))))

(defun expression-variable-p(something)
  "Something is either a symbol or an '(expt symbol number) object"
  (or (exptp something)
      (and (symbolp something)
	   (not (null something)))))

(defun monomial-expression-component-p(component)
  "T is returned when COMPONENT is a valid expression component.
COMPONENT is either a number or something validated by EXPRESSION-VARIABLE-P"
  (or (numberp component)
      (numberp (eval-as-number component))
      (expression-variable-p component))
  )

(defun monomial-expression-p(expression)
  "Validate EXPRESSION as a valid monomial expression.
EXPRESSION must begin with '* and must include only elements validated by MONOMIAL-EXPRESSION-COMPONENT-P
TODO: a numberp is a valid monomial"
  (and (listp expression)
       (equal (first expression) '*)
       (every #'identity (mapcar 'monomial-expression-component-p (rest expression))))) ;; check that all symbols are valid

(defun coefficients(expression)
  "Return all the coefficient inside EXPRESSION"
  (remove-if-not (lambda (x) (numberp (eval-as-number x))) expression))

(defun total-coefficient(expression)
  "Compute the total coefficient of EXPRESSION.
This is achived by multiplying all the coefficients of EXPRESSION"
  (reduce #'* (mapcar #'eval-as-number (coefficients expression)))) ;; default value can be omitted since for '* is 1

(defun expression-variables(expression)
  "Return all the variables inside EXPRESSION"
  (remove-if-not #'expression-variable-p expression))

(defun expression-variable-to-varpower(exp-var)
  "Convert EXP-VAR (validated by EXPRESSION-VARIABLE-P) to an object in the format of '(v exponent variable)"
  (when (expression-variable-p exp-var) ;; let's make sure
    (let ((vp-variable (if (listp exp-var)
			   (second exp-var)
			   exp-var))
	  (vp-exponent (if (listp exp-var)
			   (third exp-var)
			   1)))
      (list 'v vp-exponent vp-variable))))


(defun varpowers-from-expression(expression)
  "Return the varspowers inside EXPRESSION.
This casts the list gotted by EXPRESSION-VARIABLES."
  (mapcar #'expression-variable-to-varpower (expression-variables expression)))

(defun sort-varpowers(varpowers)
  "Sort VARPOWERS.
VARPOWERS is a list of items validated by is-varpower"
  (when (and (listp varpowers) ;;; TODO_ add better check via a varpowers-list-p 
	     (every #'identity (mapcar #'is-varpower varpowers)))
    (sort varpowers (lambda(vp1 vp2)
		      (string< (varpower-symbol vp1) (varpower-symbol vp2))))))

(defun total-degree-varpowers(varpowers)
  (when (listp varpowers) ;; TODO: add better check via a varpowers-list-p 
    (reduce #'+ (mapcar (lambda(varpower) (varpower-power varpower)) varpowers))))

(defun build-monomial-object(coefficient total-degree varpowers)
  "Given all the details builds the monomial as needed by the requirements"
  (list 'm coefficient total-degree varpowers))

(defun parse-monomial-expression(expression)
  (let ((e-coefficient (total-coefficient expression))
	(e-varpowers (sort-varpowers (varpowers-from-expression expression))));;(parse-variables expression))
    (build-monomial-object e-coefficient (total-degree-varpowers e-varpowers) e-varpowers)))

(defun as-monomial(expression)
  "EXPRESSION will be represented as a monomial.
EXPRESSION is either a number or something validated by monomial-expression-p"
  (cond ((monomial-expression-p expression) (parse-monomial-expression (rest expression)))
	((numberp expression) (build-monomial-object expression 0 ()))
	(T (error "Invalid expression"))))
