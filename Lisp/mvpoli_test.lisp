(ql:quickload "clunit")

(clunit:defsuite AsMonomialSuite ())

(clunit:deftest test-int1 (AsMonomialSuite)
    (assert-true  (= 1 -1))
    (assert-equality 4 (+ 2 2)))