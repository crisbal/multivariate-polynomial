lisp = sbcl
FILENAME ?= mvpoli

.PHONY: all
all: repl

.PHONY: test
test:
	@ echo "+++ Run tests and exit +++"
	$(lisp) --load $(FILENAME)_test.lisp

.PHONY: repl
repl:
	$(lisp) --load $(FILENAME).lisp
