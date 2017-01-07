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
  (list 'M
        coefficient
        (if (equal coefficient 0) 0 total-degree)
        (if (equal coefficient 0) NIL varpowers)))


(defun is-varpower(varpower)
  (and (listp varpower)
       (equal (list-length varpower) 3)
       (equal (first varpower) 'V)
       (numberp (second varpower))
       (symbolp (third varpower))))

(defun varpower-power(varpower)
  (when (is-varpower varpower)
    (second varpower)))

(defun varpower-symbol(varpower)
  (when (is-varpower varpower)
    (third varpower)))

(defun total-degree-varpowers(varpowers)
  (when (listp varpowers) ;; TODO: add better check via a varpowers-list-p
    (reduce #'+
            (mapcar (lambda(varpower)
                      (varpower-power varpower))
                    varpowers))))

(defun is-monomial(monomial)
  (and (listp monomial)
       (equal (list-length monomial) 4)
       (equal (first monomial) 'M)
       (numberp (second monomial))
       (numberp (third monomial))
       (>= (third monomial) 0)
       (listp (fourth monomial))
       (every #'identity
              (mapcar 'is-varpower
                      (fourth monomial)))
       (equal (total-degree-varpowers (fourth monomial)) (third monomial))))

(defun is-polynomial(polynomial)
  (and (listp polynomial)
       (equal (list-length polynomial) 2)
       (equal (first polynomial) 'POLY)
       (listp (second polynomial))
       (every #'identity (mapcar
                          'is-monomial
                          (second polynomial)))))


(defun polynomial-monomials(polynomial)
  (when (is-polynomial polynomial) ;;TODO add strict check
    (second polynomial)))

(defun compare-varpowers(vp1 vp2)
  "Comparator for varpower objects
Use case: sort list of varpower objects"
  (string< (varpower-symbol vp1) (varpower-symbol vp2)))

(defun lesser-varpower(vp1 vp2)
  "Comparator for lists of varpowers"
  (unless (and (null vp1) (null vp2))
    (let ((vp1-first-symbol (varpower-symbol (first vp1)))
	  (vp1-first-power (varpower-power (first vp1)))
	  (vp2-first-symbol (varpower-symbol (first vp2)))
	  (vp2-first-power (varpower-power (first vp2))))
      (cond ((and (null vp2) (not (null vp1))) T)
	    ((and (null vp1) (not (null vp2))) NIL)
	    ((string< vp1-first-symbol vp2-first-symbol) T)
	    ((and (equal vp1-first-power vp2-first-symbol)
		  (< vp1-first-power vp2-first-power)) T)
	    (T (lesser-varpower (rest vp1) (rest vp2)))))))

(defun exptp(something)
  "T is returned when SOMETHING is a list in the form '(expt symbol number)"
  (and (listp something)
       (equal (first something) 'expt)
       (symbolp (second something))
       (numberp (third something))
       (>= (third something) 0)))

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
       (every #'identity
              (mapcar 'monomial-expression-component-p
                      (rest expression))))) ;; check that all symbols are valid

(defun expression-coefficients(expression)
  "Return all the coefficient inside EXPRESSION"
  (remove-if-not (lambda(x)
                   (numberp (eval-as-number x)))
                 expression))

(defun total-coefficient(expression)
  "Compute the total coefficient of EXPRESSION.
This is achived by multiplying all the coefficients of EXPRESSION"
  (reduce #'*
          (mapcar #'eval-as-number
                  (expression-coefficients expression)))) ;; default value can be omitted since for '* is 1

(defun expression-variables(expression)
  "Return all the variables inside EXPRESSION"
  (remove-if-not #'expression-variable-p
                 expression))

(defun expression-variable-to-varpower(exp-var)
  "Convert EXP-VAR (validated by EXPRESSION-VARIABLE-P) to an object in the format of '(v exponent variable)"
  (when (expression-variable-p exp-var) ;; let's make sure
    (let ((vp-variable (if (listp exp-var)
			   (second exp-var)
			   exp-var))
	  (vp-exponent (if (listp exp-var)
			   (third exp-var)
			   1)))
      (build-varpower-object vp-variable
                             vp-exponent))))

(defun varpowers-from-expression(expression)
  "Return the varspowers inside EXPRESSION.
This casts the list gotted by EXPRESSION-VARIABLES."
  (mapcar #'expression-variable-to-varpower
          (expression-variables expression)))

(defun sort-varpowers(varpowers)
  "Sort VARPOWERS.
VARPOWERS is a list of items validated by is-varpower"
  (when (and (listp varpowers) ;;; TODO_ add better check via a varpowers-list-p
	     (every #'identity
                    (mapcar #'is-varpower
                            varpowers)))
    (sort (copy-seq varpowers)
          #'compare-varpowers)))



(defun compress-varpowers-reducer(element partial-list)
  (let ((e-power (varpower-power element))
	(e-symbol (varpower-symbol element))
	(first-power (varpower-power (first partial-list)))
	(first-symbol (varpower-symbol (first partial-list))))
    (cond ((equal 0 e-power)
           partial-list)
	  ((null partial-list)
           (list element))
          ((equal e-symbol first-symbol)
           (cons (build-varpower-object e-symbol
                                        (+ e-power first-power))
                 (rest partial-list)))
	  (T (cons element partial-list)))))

(defun compress-varpowers(varspowers)
  (reduce #'compress-varpowers-reducer
          varspowers
          :initial-value nil
          :from-end T))

(defun parse-monomial-expression(expression)
  (let ((e-coefficient (total-coefficient expression))
	(e-varpowers (compress-varpowers (sort-varpowers (varpowers-from-expression expression)))))
    (build-monomial-object e-coefficient
                           (total-degree-varpowers e-varpowers)
                           e-varpowers)))

(defun as-monomial(expression)
  "EXPRESSION will be represented as a monomial.
EXPRESSION is either a number or something validated by monomial-expression-p"
  (cond ((null expression)
         (build-monomial-object 0 0 NIL))
         ((monomial-expression-p
          expression)
         (parse-monomial-expression
          (rest expression)))
	((numberp expression)
         (build-monomial-object expression
                                0
                                NIL))
	(T (error "Invalid monomial expression ~A" expression))))

(defun to-monomial(generic)
  (cond ((is-monomial generic) generic)
        (T (as-monomial generic))))

(defun monomial-varpowers(monomial)
    (fourth (to-monomial monomial)))

(defun varpowers(monomial)
  "ALIAS: just because this function is a requirement"
  (monomial-varpowers monomial))

(defun monomial-degree(monomial) 
    (third (to-monomial monomial)))

(defun monomial-coefficient(monomial) 
    (second (to-monomial monomial)))

(defun monomial-variables(monomial)
  (sort (copy-seq (remove-duplicates (mapcar 'varpower-symbol
                                             (monomial-varpowers monomial))))
        #'string<))

(defun compare-monomials(mon1 mon2) 
  "Comparator for monomial objects.
Use case: sort list of monomials"
  (let ((m1-degree (monomial-degree mon1))
  (m2-degree (monomial-degree mon2))
  (m1-varpowers (monomial-varpowers mon1))
  (m2-varpowers (monomial-varpowers mon2)))
    (cond ((< m1-degree m2-degree) T)
    ((and (equal m1-degree m2-degree)
    (lesser-varpower m1-varpowers m2-varpowers)) T))))

;; BEGIN OF AS POLYNOMIAL

(defun polynomial-expression-p(expression)
  (and (listp expression)
       (equal (first expression) '+))) ;; TODO: add check for all the monomials components, will require a small adjustament of monomial-expression-p. Not really needed, since as-monomial will take care of the problem

(defun sort-monomials(monomials)
  (when (and (listp monomials) ;;; TODO_ add better check via a varpowers-list-p
	     (every #'identity
                    (mapcar #'is-monomial
                            monomials)))
    (sort (copy-seq monomials)
          #'compare-monomials)))

(defun compress-monomials-reducer(element partial-list)
  (let ((e-coefficient (monomial-coefficient element))
	(e-varpowers (monomial-varpowers element))
	(first-coefficient (monomial-coefficient (first partial-list)))
	(first-varpowers (monomial-varpowers (first partial-list)))
	(e-degree (monomial-degree element)))
    (cond ((equal 0 e-coefficient)
           partial-list)
	  ((null partial-list)
           (list element))
          ((equal e-varpowers first-varpowers)
           (if (equal 0 (+ first-coefficient e-coefficient))
               (rest partial-list)
               (cons (build-monomial-object (+ e-coefficient
                                               first-coefficient)
                                            e-degree
                                            e-varpowers)
                     (rest partial-list))))
	  (T (cons element partial-list)))))

(defun compress-monomials(monomials)
  (reduce #'compress-monomials-reducer
          monomials
          :initial-value nil
          :from-end T))

(defun build-polynomial-object(monomials)
  (list 'POLY (compress-monomials (sort-monomials monomials))))

(defun parse-polynomial-expression(expression)
  (let ((parsed-monomials (mapcar #'as-monomial
                                  expression)))
    (build-polynomial-object parsed-monomials)))

(defun as-polynomial(expression)
  "EXPRESSION will be represented as a polynomial.
EXPRESSION is a list validated by polynomial-expression-p"
  (cond ((polynomial-expression-p expression)
         (parse-polynomial-expression (rest expression)))
        ((monomial-expression-p expression)
         (build-polynomial-object (list (as-monomial expression))))
        ((numberp expression)
         (build-polynomial-object (list (as-monomial expression))))
        (T (error "Invalid polynomial expression ~A" expression))))

;; BEGIN OF HELPERS

(defun to-polynomial(generic)
  "GENERIC will be converted to a polynomial object.
All the errors should be handled by the lower level functions"
  (cond ((is-polynomial generic) generic)
	((is-monomial generic) (build-polynomial-object (list generic)))
        (T (as-polynomial generic))))


(defun vars-of(monomial)
  "ALIAS: just because this function is a requirement"
  (monomial-variables monomial))

(defun variables(generic)
  (let ((polynomial (to-polynomial generic)))
    (sort (copy-seq (remove-duplicates (reduce (lambda(partial-list monomial)
                                                 (append partial-list
                                                         (monomial-variables monomial)))
                                               (polynomial-monomials polynomial)
                                               :initial-value nil)))
          #'string<)))

(defun monomials(generic)
  (let ((polynomial (to-polynomial generic)))
    (polynomial-monomials polynomial)))

(defun coefficients(generic)
  (let ((the-coefficients (mapcar #'monomial-coefficient
                                  (monomials generic))))
    (if (null the-coefficients)
        (list 0)
        the-coefficients)))

(defun maxdegree(generic)
  "MAX-DEGREE.
Functional programming!"
  (let ((the-monomials (monomials generic)))
    (if (null the-monomials)
        0
        (reduce #'max the-monomials :key #'monomial-degree))))

(defun mindegree(generic)
  "MIN-DEGREE.
Functional programming!"
  (let ((the-monomials (monomials generic)))
    (if (null the-monomials)
        0
        (reduce #'min the-monomials :key #'monomial-degree))))

;; BEGIN OF OPERATIONS

(defun polyplus(p1 p2)
  "Compute the sum of P1 and P2
Just concat the monomials and let the reducer do the job"
  (let ((monomials1 (monomials p1))
	(monomials2 (monomials p2)))
    (build-polynomial-object (append monomials1
                                     monomials2))))

(defun monotimes(m1 m2) ;; TODO: is-monomial will need strict checking
  "Compute the product of M1 and M2, both must be monomials."
  (when (and (is-monomial m1)
	     (is-monomial m2))
    (let ((m1-coefficient (monomial-coefficient m1))
	  (m2-coefficient (monomial-coefficient m2))
	  (m1-degree (monomial-degree m1))
	  (m2-degree (monomial-degree m2))
	  (m1-varpowers (monomial-varpowers m1))
	  (m2-varpowers (monomial-varpowers m2)))
      (build-monomial-object (* m1-coefficient
                                m2-coefficient)
                             (+ m1-degree
                                m2-degree)
                             (compress-varpowers (sort-varpowers (append m1-varpowers
                                                                         m2-varpowers)))))))

(defun monotimespoly(mono poly-generic) ;; TODO: add sort and compress? Is it needed?
  ""
  (when (is-monomial mono) ;; TODO: will need strict checking
    (let ((the-monomials (monomials poly-generic))) 
      (build-polynomial-object (mapcar (lambda(mono-of-poly)
                                         (monotimes mono-of-poly
                                                    mono))
                                       the-monomials)))))

(defun polyminus(p1-generic p2-generic)
  "Compute the difference of two polynomials."
  (let ((p1 (to-polynomial p1-generic))
        (negated-p2 (monotimespoly (as-monomial -1)
                                   (to-polynomial p2-generic))))
    (polyplus p1 negated-p2)))

(defun polytimes(p1-generic p2-generic)
  "Compute the product of two polynomials"
  (let ((monomials-of-p1 (monomials p1-generic))
        (p2 (to-polynomial p2-generic)))
    (build-polynomial-object
     (reduce (lambda(list-until-now mono-of-poly1)
               (append (second (monotimespoly mono-of-poly1
                                              p2))
                       list-until-now))
             monomials-of-p1
             :initial-value nil))))

;; BEGIN OF POLYVAL

(defun get-value-for-symbol(symbol var-to-value-assoc)
  (cdr (assoc symbol var-to-value-assoc)))

(defun varpower-values-reducer(partial-value varpower var-to-value-assoc)
  (let ((v-symbol (varpower-symbol varpower))
        (v-power (varpower-power varpower)))
    (* partial-value
       (expt (get-value-for-symbol v-symbol
                                   var-to-value-assoc)
             v-power))))

(defun monomial-value(monomial var-to-value-assoc)
  (* (monomial-coefficient monomial)
     (reduce (lambda(partial-value varpower)
               (varpower-values-reducer partial-value
                                        varpower
                                        var-to-value-assoc))
             (monomial-varpowers monomial)
             :initial-value 1)))

(defun monomial-values-reducer(partial-value monomial var-to-value-assoc)
  (+ partial-value (monomial-value monomial
                                   var-to-value-assoc)))

(defun polyval(polynomial-generic values-for-vars)
  (let ((var-to-value-assoc (pairlis (variables polynomial-generic)
                                     values-for-vars)))
    (reduce (lambda(partial-value monomial)
              (monomial-values-reducer partial-value monomial
                                       var-to-value-assoc))
            (second (to-polynomial polynomial-generic))
            :initial-value 0)))

;; PPRINT

(defun pprint-varpower(varpower &optional head)
 (when (is-varpower varpower)
       (let ((symbol (varpower-symbol varpower))
             (power (varpower-power varpower)))
         (progn (unless head
                  (format T "*"))
                (format T "~A" symbol)
                (unless (= power 1)
                  (format T "^~A" power))
                NIL)))) ;;we return nil explicitally, it is not needed but let's be clear

(defun pprint-monomial(monomial &optional head) ;; TODO: add strict is-monomial
  (when (is-monomial monomial)
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
               (mapcar 'pprint-varpower
                       (rest varpowers)))
             NIL)))) ;;here we return nil explicitally because the unexpected return value of mapcar (we know it is nil but better safe than sorry)
  
(defun pprint-polynomial(polynomial)
  (let ((the-monomials (to-polynomial polynomial)))
    (if (null the-monomials)
	(format T "0")
	(progn (pprint-monomial (first the-monomials) T)
	       (mapcar #'pprint-monomial
                       (rest the-monomials))
	       NIL)))) ;; again force nil return value, we know it is nil but let's be safe and follow requirement


