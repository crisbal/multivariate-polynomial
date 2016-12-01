(defun as-varpower(vp-expression-list)
  (if (and (listp vp-expression-list)
	   (eq (first vp-expression-list) 'expt)
	   (symbolp (second vp-expression-list))
	   (numberp (third vp-expression-list)))
      (list 'V (second vp-expression-list) (third vp-expression-list))))

(defun to-monomial-component(element)
  (cond ((numberp element) element)
	((symbolp element) (as-varpower (list 'expt element '1)))
	((as-varpower element) (as-varpower element))
	(T (error "What is that parsed thing?"))))

(defun as-monomial-parse(monomial-expression)
  (if (null monomial-expression)
       nil
       (cons (to-monomial-component (first monomial-expression)) (as-monomial-parse (rest monomial-expression)))))

(defun as-monomial(monomial-expression)
  (if (and (listp monomial-expression)
	   (eq (first monomial-expression) '*))
      (as-monomial-parse (rest monomial-expression))))


