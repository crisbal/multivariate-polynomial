;;;; XXXNUMBER XXXSURNAME XXXNAME
;;;; XXXCO_NUMBER XXXCO_SURNAME XXXCO_NAME

(defun eval-as-number(expression)
  "Eval EXPRESSION and return its numeric value
It works by trying to eval the expression, catching any error or warning,
and returning a number only if the evalued result is a number, NIL otherwise"
  (let ((result (handler-case (eval expression)
                  (error () nil)
                  (warning () nil))))
    (if (numberp result) result nil)))

(defun build-varpower-object(base exponent)
  "Build a varpower object '(V EXPONENT BASE)"
  (list 'V exponent base))



(defun is-varpower(varpower)
  "Check if VARPOWER is a varpower object"
  (and (listp varpower)
       (equal (list-length varpower) 3)
       (equal (first varpower) 'V)
       (numberp (second varpower))
       (symbolp (third varpower))))

(defun varpower-power(varpower)
  "Gets the power from VARPOWER "
  (when (is-varpower varpower)
    (second varpower)))

(defun varpower-symbol(varpower)
  "Gets the symbol from VARPOWER"
  (when (is-varpower varpower)
    (third varpower)))

(defun total-degree-varpowers(varpowers)
  "Compute total degree for a given list of VARPOWERS
REDUCE is used to compute the sum"
  (when (listp varpowers) ;; TODO: add better check via a varpowers-list-p
    (reduce #'+
            (mapcar (lambda(varpower)
                      (varpower-power varpower))
                    varpowers))))

(defun assert-or-error(condition error-message-args)
  "Assert condition or throw an error formatted by ERROR-MESSAGE-ARGS"
  (if condition
      T
      ;; we use apply becasue format has a variable number of args
      (apply #'error error-message-args)))

(defun is-monomial(monomial)
  "Check if MONOMIAL is a monomial object. NIL is returned if it is not.
An error is thrown if it is a monomial object but it's malformed in any way"
  (and (listp monomial)
       (equal (first monomial) 'M)
       (assert-or-error (equal (list-length monomial) 4)
                        (list "Invalid length for monomial object: ~A"
                              monomial))
       (assert-or-error (numberp (second monomial))
                        (list "coefficient not a number in: ~A"
                              monomial))
       (assert-or-error (numberp (third monomial))
                        (list "total-degree not a number in: ~A"
                              monomial))
       (assert-or-error (>= (third monomial) 0)
                        (list "total-degree not positive in: ~A"
                              monomial))
       (assert-or-error (listp (fourth monomial))
                        (list "expected vps as list in: ~A"
                              monomial))
       (assert-or-error (every #'identity
                               (mapcar 'is-varpower
                                       (fourth monomial)))
                        (list "Invalid varpower in monomial: ~A"
                              monomial))
       (assert-or-error (equal (total-degree-varpowers (fourth monomial))
                               (third monomial))

                        (list "Invalid total-degree for monomial: ~A"
                              monomial))))

(defun is-polynomial(polynomial)
  (and (listp polynomial)
       (equal (first polynomial) 'POLY)
       (assert-or-error (equal (list-length polynomial) 2)
                        (list "Invalid polynomial, length not 2 for: ~A"
                              polynomial))
       (assert-or-error (listp (second polynomial))
                        (list "Invalid polynomial, 2nd item not a list for: ~A"
                              polynomial))
       (assert-or-error (every #'identity
                               (mapcar 'is-monomial
                                       (second polynomial)))
                        (list "Invalid monomial in polynomial: ~A"
                              polynomial))))


(defun polynomial-monomials(polynomial)
  "Getter for the monomials of a polynomial"
  (when (is-polynomial polynomial)
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
      (cond ((and (null vp2)
                  (not (null vp1))) T)
            ((and (null vp1)
                  (not (null vp2))) NIL)
            ((string< vp1-first-symbol vp2-first-symbol) T)
            ((string> vp1-first-symbol vp2-first-symbol) nil)
            ((and (equal vp1-first-symbol
                         vp2-first-symbol)
                  (< vp1-first-power vp2-first-power)) T)
            ((and (equal vp1-first-symbol
                         vp2-first-symbol)
                  (> vp1-first-power vp2-first-power)) nil)
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
EXPRESSION must begin with '* and must include only elements
validated by MONOMIAL-EXPRESSION-COMPONENT-P"
  (and (listp expression)
       (equal (first expression) '*)
       ;; check if the value of monomial-expression-component-p applied to each
       ;; element of the third element is true
       (every #'monomial-expression-component-p
              (rest expression)))) ;; check that all symbols are valid

(defun expression-coefficients(expression)
  "Return all the coefficient inside EXPRESSION"
  ;; remove element of the list if (numberp  (eval-as-number x)) is not true
  (remove-if-not (lambda(x)
                   (numberp (eval-as-number x)))
                 expression))

(defun total-coefficient(expression)
  "Compute the total coefficient of EXPRESSION.
This is achived by multiplying all the coefficients of EXPRESSION"
  (reduce #'*
          (mapcar #'eval-as-number
                  (expression-coefficients expression))))
;; default value for REDUCE can be omitted since for '* is 1

(defun expression-variables(expression)
  "Return all the variables inside EXPRESSION"
  (remove-if-not #'expression-variable-p
                 expression))

(defun expression-variable-to-varpower(exp-var)
  "Convert EXP-VAR to a varpower"
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
This casts the list gotten by EXPRESSION-VARIABLES."
  (mapcar #'expression-variable-to-varpower
          (expression-variables expression)))

(defun sort-varpowers(varpowers)
  "Sort VARPOWERS.
VARPOWERS is a list of items validated by is-varpower"
  (when (and (listp varpowers) ;;; TODO add better check via a varpowers-list-p
             (every #'identity
                    (mapcar #'is-varpower
                            varpowers)))
    (sort (copy-seq varpowers)
          #'compare-varpowers)))

(defun compress-varpowers-reducer(element partial-list)
  "To be used with REDUCE, merge duplicates and remove null varpowers"
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
          :initial-value nil ;empty list
          :from-end T)) ;start from end since the varpowers are sorted

(defun build-monomial-object(coefficient total-degree varpowers)
  "Given all the details builds the monomial as needed by the requirements
It takes care of compressing and sorting"
  (list 'M
        coefficient
        (if (equal coefficient 0)
            0
            total-degree)
        (if (equal coefficient 0)
            NIL
            (compress-varpowers (sort-varpowers varpowers)))))

(defun parse-monomial-expression(expression)
  "Extract the needed elements from expression and build the monomial"
  (let ((e-coefficient (total-coefficient expression))
        (e-varpowers (varpowers-from-expression expression)))
    (build-monomial-object e-coefficient
                           (total-degree-varpowers e-varpowers)
                           e-varpowers)))

(defun as-monomial(expression)
  "EXPRESSION will be represented as a monomial.
EXPRESSION is either a number or something validated by monomial-expression-p
NIL is also a valid expression treated as the null monomial 0
An error is thrown if the monomial expression is malformed in any way"
  (cond ((null expression) ;; null monomial = 0
         (build-monomial-object 0 0 NIL))
        ((monomial-expression-p expression)
         (parse-monomial-expression (rest expression)))
        ((and (symbolp expression) ;; a symbol is a valid monomial
              (not (null expression)))
         (build-monomial-object 1 
                                1
                                (list (build-varpower-object expression 1))))
        ((numberp expression) ;; a number is a valid monomial expression
         (build-monomial-object expression
                                0
                                NIL))
        (T (error "Invalid monomial expression ~A"
                  expression))))

(defun adjust-monomial(monomial)
  "To reorder the vps in case the passed monomial had wrong order"
  (when (is-monomial monomial)
    (let ((coefficient (second monomial))
          (total-degree (third monomial))
          (vps (fourth monomial)))
      (build-monomial-object coefficient total-degree vps))))

(defun to-monomial(generic)
  "Take a GENERIC object (expression or monomial) and convert it to monomial
If GENERIC is a monomial it will be sorted and compressed"
  (cond ((is-monomial generic) (adjust-monomial generic))
        (T (as-monomial generic))))

(defun monomial-varpowers(monomial)
  "Get the varpower of a MONOMIAL generic"
  (fourth (to-monomial monomial)))

(defun varpowers(monomial)
  "ALIAS: just because this function is a requirement
We us monomial-varpowers because it has a clearer name"
  (monomial-varpowers monomial))

(defun monomial-degree(monomial)
  "Get the degree of a MONOMIAL generic"
  (third (to-monomial monomial)))

(defun monomial-coefficient(monomial)
  "Get the coefficient of a MONOMIAL generic"
  (second (to-monomial monomial)))

(defun monomial-variables(monomial)
  "Get the variables of a MONOMIAL generic
COPY-SEQ it is maybe not needed but better safe than sorry.
VARS are sorted ascending by the STRING< function"
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
  "Check if EXPRESSION looks like a valid polynomial-expression
We only check the external level and"
  (and (listp expression)
       (equal (first expression) '+)))

(defun sort-monomials(monomials)
  "Sort MONOMIALS"
  (when (and (listp monomials) ;; TODO add better check via a varpowers-list-p
             (every #'is-monomial
                    monomials))
    (sort (copy-seq monomials) ;; copyseq because sort is destructive
          #'compare-monomials)))

(defun compress-monomials-reducer(element partial-list)
  "Use with REDUCE. Merge duplicates, remove null monomials.
Very similar to compress-varpower-reducer"
  (let ((e-coefficient (monomial-coefficient element))
        (e-varpowers (monomial-varpowers element))
        (first-coefficient (monomial-coefficient (first partial-list)))
        (first-varpowers (monomial-varpowers (first partial-list)))
        (e-degree (monomial-degree element)))
    (cond ((equal 0 e-coefficient) ;; null monomial
           partial-list)
          ((null partial-list)
           (list element)) ;; base case
          ((equal e-varpowers first-varpowers) ;; merge on equal varpowers
           (if (equal 0 (+ first-coefficient e-coefficient))
               ;; skip merge if resulting monomial is 0
               (rest partial-list)
               (cons (build-monomial-object (+ e-coefficient
                                               first-coefficient)
                                            e-degree
                                            e-varpowers)
                     (rest partial-list))))
          (T (cons element partial-list)))))

(defun compress-monomials(monomials)
  "Compress MONOMIALS
Very similar to compress-varpowers"
  (reduce #'compress-monomials-reducer
          monomials
          :initial-value nil
          :from-end T))

(defun build-polynomial-object(monomials)
  "Return a POLY object
Takes care of sorting and compressing too."
  (list 'POLY (compress-monomials (sort-monomials monomials))))

(defun parse-polynomial-expression(expression)
  "Parse EXPRESSION ('+ is already removed)
We don't really do much, just parse with as-monomial"
  (let ((parsed-monomials (mapcar #'as-monomial
                                  expression)))
    (build-polynomial-object parsed-monomials)))

(defun as-polynomial(expression)
  "EXPRESSION will be represented as a polynomial.
EXPRESSION is a list validated by polynomial-expression-p or
EXPRESSION could also be a number or a valid monomial expression"
  (cond ((polynomial-expression-p expression)
         (parse-polynomial-expression (rest expression)))
        ((monomial-expression-p expression) ;; maybe it is a monomial, accept it
         (build-polynomial-object (list (as-monomial expression))))
        ((numberp expression) ;; maybe it si a number, accept it
         (build-polynomial-object (list (as-monomial expression))))
        ((null expression)
         (build-polynomial-object NIL))
        ((and (symbolp expression) ;; maybe it is a symbol, accept it
              (not (null expression)))
         (build-polynomial-object (list (as-monomial expression))))
        (T (error "Invalid polynomial expression ~A"
                  expression))))

(defun adjust-polynomial(polynomial)
  "Take care of reordering POLYNOMIAL
Also takes care of reordering its monomials"
  (when (is-polynomial polynomial)
    (build-polynomial-object (mapcar 'to-monomial
                                     (polynomial-monomials polynomial)))))

(defun to-polynomial(generic)
  "GENERIC will be converted to a polynomial object.
All the errors should be handled by the lower level functions"
  (cond ((is-polynomial generic) (adjust-polynomial generic))
        ((is-monomial generic) (build-polynomial-object (list (to-monomial generic))))
        (T (as-polynomial generic))))

(defun vars-of(monomial)
  "ALIAS: just because this function is a requirement
See MONOMIAL-VARIABLES"
  (monomial-variables monomial))

(defun variables(generic)
  "Get the variables in the GENERIC polynomial object
The reduce is needed as a kind of list flattern"
  (let ((polynomial (to-polynomial generic)))
    (sort (copy-seq (remove-duplicates (reduce #'append
                                               (polynomial-monomials polynomial)
                                               :initial-value NIL
                                               :key #'monomial-variables
                                               ;; call monomial-variables
                                               ; on each item
                                               )))
          #'string<))) ;; sort ascending

(defun monomials(generic)
  "Get the monomials of polynomial object GENERIC
POLYNOMIAL-MONOMIALS does not accept GENERIC, only polynomials"
  (let ((polynomial (to-polynomial generic)))
    (polynomial-monomials polynomial)))

(defun coefficients(generic)
  "Get the list of coefficients for GENERIC"
  (let ((the-coefficients (mapcar #'monomial-coefficient
                                  (monomials generic))))
    (if (null the-coefficients)
        (list 0)
        the-coefficients)))

(defun maxdegree(generic)
  "MAX-DEGREE.
REDUCE to the rescue!"
  (let ((the-monomials (monomials generic)))
    (if (null the-monomials)
        0
        ;; :key is the function that will be applied to each monomial
        ;; before applying #'max
        (reduce #'max the-monomials :key #'monomial-degree))))

(defun mindegree(generic)
  "MIN-DEGREE.
REDUCE again"
  (let ((the-monomials (monomials generic)))
    (if (null the-monomials)
        0
        (reduce #'min the-monomials :key #'monomial-degree))))

;; BEGIN OF OPERATIONS

(defun polyplus(p1 p2)
  "Compute the sum of P1 and P2, generic polynomials
Just concat the monomials and let the reducer do the job"
  (let ((monomials1 (monomials p1))
        (monomials2 (monomials p2)))
    (build-polynomial-object (append monomials1
                                     monomials2))))

(defun monotimes(m1 m2)
  "Compute the product of M1 and M2, both must be monomials."
  (when (and (is-monomial m1)
             (is-monomial m2))
    (let ((m1-coefficient (monomial-coefficient m1))
          (m2-coefficient (monomial-coefficient m2))
          (m1-degree (monomial-degree m1))
          (m2-degree (monomial-degree m2))
          (m1-varpowers (monomial-varpowers m1))
          (m2-varpowers (monomial-varpowers m2)))
      (build-monomial-object (* m1-coefficient  ;;pretty straight-forward
                                m2-coefficient) ;; multiply coeffs
                             (+ m1-degree ;; sum degrees
                                m2-degree)
                             (append m1-varpowers ;;append varpowers
                                     m2-varpowers)))))

(defun monotimespoly(mono poly-generic)
  "Multiply a monomial for a polynomial. Will return a polynomial"
  (when (is-monomial mono)
    (let ((the-monomials (monomials poly-generic)))
      ;; just call monotimes as needed
      (build-polynomial-object (mapcar (lambda(mono-of-poly)
                                         (monotimes mono-of-poly
                                                    mono))
                                       the-monomials)))))

(defun polyminus(p1-generic p2-generic)
  "Compute the difference of two generic polynomials."
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
  "Get the value associated with SYMBOL in the assoc VAR-TO-VALUE-ASSOC"
  ;; cdr becaue assoc returns a tuple/cons (key . value), we need value
  (cdr (assoc symbol var-to-value-assoc)))


(defun varpower-values-reducer(partial-value varpower var-to-value-assoc)
  "Use with REDUCE.
Given a VARPOWER and a PARTIAL-VALUE computes the product.
VAR-TO-VALUE-ASSOC is the map that maps a variable to its value"
  (let ((v-symbol (varpower-symbol varpower))
        (v-power (varpower-power varpower)))
    (* partial-value ;; multiply current value by
       (expt (get-value-for-symbol v-symbol ;we want to compute a power v^e
                                   var-to-value-assoc)
             v-power))))

(defun monomial-value(monomial var-to-value-assoc)
  "Evaluate the monomial in the points specified by VAR-TO-VALUE-ASSOC"
  (* (monomial-coefficient monomial)
     ;; this reduce seems pretty strange but we use the lambda because
     ;; we need to pass the additional VAR-TO-VALUE-ASSOC to the reducer
     ;; so we can extract the associated values, since REDUCE does
     ;; not pass additinal parameters to the reduce function
     (reduce (lambda(partial-value varpower)
               (varpower-values-reducer partial-value
                                        varpower
                                        var-to-value-assoc))
             (monomial-varpowers monomial) ;; compute on the varpowers
             :initial-value 1))) ;; no varpowers = 1

(defun monomial-values-reducer(partial-value monomial var-to-value-assoc)
  "USE with REDUCE.
Compute the value of the current monomial and add it to the partial value"
  (+ partial-value (monomial-value monomial
                                   var-to-value-assoc)))

(defun polyval(polynomial-generic values-for-vars)
  "Evaluate POLYNOMIAL-GENERIC in the points specified by VALUES-FOR-VARS
VALUES-FOR-VARS needs to be of at least the length of (VARIABLES poly-generic)
A SIMPLE-ERROR is thrown on less values-for-vars than variables."
  (let ((vars (variables polynomial-generic)))
    (if (> (length vars)
           (length values-for-vars)) ;; check for the exact number of values
        (error "Invalid length for supplied list of values")
        ;; pairlis builds the assoc/map between variable and values
        ;; we use subseq to handle the case that more values than
        ;; variables were supplied
        ;; (x y) (1 2 3) => ((x 1) (y 2))
        (let ((var-to-value-assoc (pairlis vars
                                           (subseq values-for-vars
                                                   0
                                                   (length vars)))))
          ;; same as monomail-value, this lambda is for passing
          ;; VAR-TO-VALUE-ASSOC to the reducer
          (reduce (lambda(partial-value monomial)
                    (monomial-values-reducer partial-value
                                             monomial
                                             var-to-value-assoc))
                  (polynomial-monomials (to-polynomial polynomial-generic))
                  :initial-value 0))
        )))

;; PPRINT

;; In all the pprint progn is used since it is the best way
;; (in our opinion) to handle printing operations without
;; going crazy with lists, recursion, and many other solutions

(defun pprint-varpower(varpower &optional head)
  "Pretty print VARPOWER
Pass HEAD if it is the first varpower and you don't want to print the * before"
  (when (is-varpower varpower)
    (let ((symbol (varpower-symbol varpower))
          (power (varpower-power varpower)))
      (progn (unless head ;; if it is not the first of the list of vps
               (format T "*"))
             (format T "~A" symbol)
             (unless (= power 1) ;; don't print ^1
               (format T "^~A" power))
             NIL)))) ;;we return nil explicitally
;; it is not needed but let's be clear

(defun pprint-monomial(monomial &optional head)
  "Pretty print MONOMIAL
Pass HEAD if you don't want to print + before the monomial
HEAD if for the head monomial"
  (when (is-monomial monomial)
    (let ((coefficient (monomial-coefficient monomial))
          (acoefficient (abs (monomial-coefficient monomial)))
          (varpowers (monomial-varpowers monomial)))
      (progn (if (< coefficient 0) ;;on negative coefficient always print -
                 (format T " - ")
                 (unless head ;; only print + if it is not the first
                   (format T " + ")))
             (when (or (not (= acoefficient 1)) ;; print not 1 coefficients
                       (null varpowers)) ;; or force print if varspower is null
               (format T "~A" acoefficient))
             (unless (null varpowers)
               (unless (= acoefficient 1) (format T "*"))
               (pprint-varpower (first varpowers) T) ;; print the HEAD VP
               (mapcar 'pprint-varpower ;; print the others
                       (rest varpowers)))
             NIL)))) ;;here we return nil explicitally
;; because the unexpected return value of mapcar
;; (we know it is nil but better safe than sorry)

(defun pprint-polynomial(polynomial)
  "Pretty print a POLYNOMIAL generic"
  (let ((the-monomials (monomials polynomial)))
    (if (null the-monomials)
        (format T "0") ;; no monomial = 0
        (progn (pprint-monomial (first the-monomials) T) ;; print the HEAD
               (mapcar #'pprint-monomial ;; print the others
                       (rest the-monomials))
               NIL)))) ;; again force nil return value
;; we know it is nil but let's be safe and follow requirement
