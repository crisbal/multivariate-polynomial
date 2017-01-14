lisp = rlwrap sbcl
FILENAME ?= mvpoli

.PHONY: all
all: repl

.PHONY: test
test:
	@ echo "+++ Run tests and exit +++"
	$(lisp) --noinform --load ${FILENAME}.lisp --script $(FILENAME)_test.lisp

.PHONY: test_interactive
test_interactive:
	$(lisp) --noinform --load $(FILENAME)_test.lisp --eval "(format t \"~%~%To get more details run (run-tests)\")"

.PHONY: repl
repl:
	$(lisp) --noinform --load $(FILENAME).lisp
