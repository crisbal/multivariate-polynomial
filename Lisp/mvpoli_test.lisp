(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "clunit")

(clunit:defsuite mvpoli ())
(clunit:defsuite as-monomial-suite (mvpoli))

(clunit:deftest test-int1 (as-monomial-suite)
  (clunit:assert-true  (= 1 1)))

(defun run-tests() 
	(clunit:run-suite 'mvpoli :report-progress T))

(run-tests)