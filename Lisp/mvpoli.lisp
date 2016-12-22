;; TODO is-monomial
;; TODO is-polynomial
;; TODO monomials
;; TODO coefficients
;; TODO variables
;; TODO polyminus
;; TODO polyval

(defun eval-as-number(expression)
  "Eval EXPRESSION and return its numeric value"  
  (let ((result (handler-case (eval expression)
		  (error () nil)
		  (warning () nil))))
    (if (numberp result) result nil)))

(defun build-varpower-object(base exponent)
  (list 'V exponent base))

(defun build-monomial-object(coefficient total-degree varpowers)
  "Given all the details builds the monomial as needed by the requirements"
  (list 'M coefficient total-degree varpowers))

(defun build-polynomial-object(monomials)
  (list 'P monomials))

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

(defun is-monomial(monomial)
  (warn "PLEASE IMPLEMENT IS-MONOMIAL")
  T)

(defun is-polynomial(polynomial)
  (warn "PLEASE IMPLEMENT IS-POLYNOMIAL")
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
  "Comparator for varpower objects.
Use case: sort list of varpower objects"  
  (string< (varpower-symbol vp1) (varpower-symbol vp2)))

(defun lesser-varpower(vp1 vp2)
  "Comparator for lists of varpowers"
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
  "Comparator for monomial objects.
Use case: sort list of monomials"
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
      (build-varpower-object vp-variable vp-exponent))))

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


(defun compress-varpowers-reducer(element partial-list)
  (let ((e-power (varpower-power element))
	(e-symbol (varpower-symbol element))
	(first-power (varpower-power (first partial-list)))
	(first-symbol (varpower-symbol (first partial-list))))
    (cond ((null partial-list) (list element))
	  ((equal 0 e-power) partial-list)
	  ((equal e-symbol first-symbol) (cons (build-varpower-object e-symbol (+ e-power first-power)) (rest partial-list)))
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
    (build-polynomial-object parsed-monomials)))

(defun as-polynomial(expression)
  "EXPRESSION will be represented as a polynomial.
EXPRESSION is a list validated by polynomial-expression-p"
  (if (polynomial-expression-p expression)
      (parse-polynomial-expression (rest expression))
      (error "Invalid polynomial expression ~A" expression)))

(defun polyplus(p1 p2)
  (let ((monomials1 (second p1)) ;; TODO: replace with monomials
	(monomials2 (second p2)))
    (build-polynomial-object (compress-monomials (sort-monomials (append monomials1 monomials2))))))

(defun monotimes(m1 m2)
  (when (and (is-monomial m1)
	     (is-monomial m2))
    (let ((m1-coefficient (monomial-coefficient m1))
	  (m2-coefficient (monomial-coefficient m2))
	  (m1-degree (monomial-degree m1))
	  (m2-degree (monomial-degree m2))
	  (m1-varpowers (monomial-varpowers m1))
	  (m2-varpowers (monomial-varpowers m2)))
      (build-monomial-object (* m1-coefficient m2-coefficient) (+ m1-degree m2-degree) (compress-varpowers (sort-varpowers (append m1-varpowers m2-varpowers)))))))

(defun monotimespoly(mono poly) ;; TODO: add sort and compress? Is it needed? 
  (when (and (is-polynomial poly) ;; TODO: replace with is-polynomial
	     (is-monomial mono))
    (let ((monomials (second poly))) ;; TODO: use polynomial-monomials
      (build-polynomial-object (mapcar (lambda(mono-of-poly) (monotimes mono-of-poly mono)) monomials)))))

(defun polytimes(p1 p2)
  (when (and (is-polynomial p1)
	     (is-polynomial p2))
    (let ((monomials-of-p1 (second p1))) ;; TODO: use polynomial-monomials
       (build-polynomial-object (compress-monomials (sort-monomials (reduce (lambda(list-until-now mono-of-poly1) (append (second (monotimespoly mono-of-poly1 p2)) list-until-now)) monomials-of-p1 :initial-value nil)))))))

(defun pprint-varpower(varpower &optional head)
  (let ((symbol (varpower-symbol varpower))
	(power (varpower-power varpower)))
    (progn (unless head
	     (format T "*"))
	   (format T "~A" symbol)
	   (unless (= power 1)
	     (format T "^~A" power))
	   NIL))) ;;we return nil explicitally, it is not needed but let's be clear

(defun pprint-monomial(monomial &optional head)
  (let ((coefficient (monomial-coefficient monomial))
	(acoefficient (abs (monomial-coefficient monomial)))
	(varpowers (monomial-varpowers monomial)))
    (progn (unless head
	     (if (< coefficient 0)
		 (format T " - ")
		 (format T " + ")))
	   (unless (= acoefficient 1)
	     (format T "~A" coefficient)
	     (unless (null varpowers)
	       (format T "*")))
	   (unless (null varpowers)
	     (pprint-varpower (first varpowers) T)
	     (mapcar 'pprint-varpower (rest varpowers)))
	   NIL))) ;;here we return nil explicitally because the unexpected return value of mapcar (we know it is nil but better safe than sorry)
  
(defun pprint-polynomial(polynomial)
  (let ((monomials (second polynomial)))
    (if (null monomials)
	(format T "0")
	(progn (pprint-monomial (first monomials) T)
	       (mapcar #'pprint-monomial (rest monomials))
	       NIL)))) ;; again force nil return value, we know it is nil but let's be safe and follow requirement

