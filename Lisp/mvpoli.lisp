(defun eval-as-number(expression)
  "Eval EXPRESSION and return its numeric value"  
  (let ((result (handler-case (eval expression)
		  (error () nil)
		  (warning () nil))))
    (if (numberp result) result nil)))

(defun is-varpower(varpower)
  (and (equal (list-length varpower) 3)
    (equal (first varpower) 'V)
    (numberp (second varpower))
    (not (numberp (third varpower)))))

(defun varpower-power(varpower)  ;;TODO add check for is-varpower
  (when (and (listp varpower)
	     (numberp (second varpower)))
    (second varpower)))

(defun varpower-symbol(varpower) ;; TODO add check for is-varpower
  (when (and (listp varpower)
	     (symbolp (third varpower)))
    (third varpower)))

(defun is-monomial(monomials)
  (warn "PLEASE IMPLEMENT IS-MONOMIAL")
  T)

(defun monomial-varpowers(monomial) ;; TODO: add check for is-monomial
  (when (and (listp monomial)
	     (listp (fourth monomial)))
    (fourth monomial)))

(defun monomial-degree(monomial) ;; TODO: add check for is-monomial
  (when (and (listp monomial)
	     (numberp (third monomial)))
    (third monomial)))

(defun monomial-coefficient(monomial) ;; TODO: add check for is-monomial
  (when (and (listp monomial)
	     (numberp (second monomial)))
    (second monomial)))

(defun compare-varpowers(vp1 vp2)
  (string< (varpower-symbol vp1) (varpower-symbol vp2)))

(defun lesser-varpower(vp1 vp2)
  (unless (or (null vp1) (null vp2)) 
    (let ((vp1-first-symbol (varpower-symbol (first vp1)))
	  (vp1-first-power (varpower-power (first vp1)))
	  (vp2-first-symbol (varpower-symbol (first vp2)))
	  (vp2-first-power (varpower-power (first vp2))))
      (cond ((string< vp1-first-symbol vp2-first-symbol) T)
	    ((and (equal vp1-first-power vp2-first-symbol)
		  (< vp1-first-power vp2-first-power)) T)
	    (T (lesser-varpower (rest vp1) (rest vp2)))))))

(defun compare-monomials(mon1 mon2) ;; TODO: implement on equal degree, just like prolog 
  (let ((m1-degree (monomial-degree mon1))
	(m2-degree (monomial-degree mon2))
	(m1-varpowers (monomial-varpowers mon1))
	(m2-varpowers (monomial-varpowers mon2)))
    (cond ((< m1-degree m2-degree) T)
	  ((lesser-varpower m1-varpowers m2-varpowers) T))))

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
      (expression-variable-p component)))

(defun monomial-expression-p(expression)
  "Validate EXPRESSION as a valid monomial expression.
EXPRESSION must begin with '* and must include only elements validated by MONOMIAL-EXPRESSION-COMPONENT-P"
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
    (sort varpowers #'compare-varpowers)))

(defun total-degree-varpowers(varpowers)
  (when (listp varpowers) ;; TODO: add better check via a varpowers-list-p 
    (reduce #'+ (mapcar (lambda(varpower) (varpower-power varpower)) varpowers))))

(defun build-monomial-object(coefficient total-degree varpowers)
  "Given all the details builds the monomial as needed by the requirements"
  (list 'm coefficient total-degree varpowers))

(defun compress-varpowers-reducer(element partial-list)
  (let ((e-power (varpower-power element))
	(e-symbol (varpower-symbol element))
	(first-power (varpower-power (first partial-list)))
	(first-symbol (varpower-symbol (first partial-list))))
    (cond ((null partial-list) (list element))
	  ((equal 0 e-power) partial-list)
	  ((equal e-symbol first-symbol) (cons (list 'V (+ e-power first-power) e-symbol) (rest partial-list)))
	  (T (cons element partial-list)))))

(defun compress-varpowers(varspowers)
  (reduce #'compress-varpowers-reducer varspowers :initial-value nil :from-end T))

(defun parse-monomial-expression(expression)
  (let ((e-coefficient (total-coefficient expression))
	(e-varpowers (compress-varpowers (sort-varpowers (varpowers-from-expression expression)))))
    (build-monomial-object e-coefficient (total-degree-varpowers e-varpowers) e-varpowers)))

(defun as-monomial(expression)
  "EXPRESSION will be represented as a monomial.
EXPRESSION is either a number or something validated by monomial-expression-p"
  (cond ((monomial-expression-p expression) (parse-monomial-expression (rest expression)))
	((numberp expression) (build-monomial-object expression 0 nil))
	(T (error "Invalid monomial expression ~A" expression))))

;; BEGIN OF AS POLYNOMIAL

(defun polynomial-expression-p(expression)
  (and (listp expression)
       (equal (first expression) '+))) ;; TODO: add check for all the monomials components, will require a small adjustament of monomial-expression-p. Not really needed, since as-monomial will take care of the problem

(defun sort-monomials(monomials)
  (when (and (listp monomials) ;;; TODO_ add better check via a varpowers-list-p 
	     (every #'identity (mapcar #'is-monomial monomials)))
    (sort monomials #'compare-monomials)))

(defun compress-monomials-reducer(element partial-list)
  (let ((e-coefficient (monomial-coefficient element))
	(e-varpowers (monomial-varpowers element))
	(first-coefficient (monomial-coefficient (first partial-list)))
	(first-varpowers (monomial-varpowers (first partial-list)))
	(e-degree (monomial-degree element)))
    (cond ((null partial-list) (list element))
	  ((equal 0 e-coefficient) partial-list)
	  ((equal e-varpowers first-varpowers) (cons (build-monomial-object (+ e-coefficient first-coefficient) e-degree e-varpowers) (rest partial-list)))
	  (T (cons element partial-list)))))

(defun compress-monomials(monomials)
  (reduce #'compress-monomials-reducer monomials :initial-value nil :from-end T))

(defun parse-polynomial-expression(expression)
  (let ((parsed-monomials (compress-monomials (sort-monomials (mapcar #'as-monomial expression)))))
    (list 'P parsed-monomials)))

(defun as-polynomial(expression)
  "EXPRESSION will be represented as a polynomial.
EXPRESSION is a list validated by polynomial-expression-p"
  (if (polynomial-expression-p expression)
      (parse-polynomial-expression (rest expression))
      (error "Invalid polynomial expression ~A" expression)))


