PROLOG = swipl
FILENAME ?= mvpoli

.PHONY: all
all: repl

.PHONY: test
test:
	@ echo "+++ Run tests and exit +++"
	$(PROLOG) -s $(FILENAME) -g $(FILENAME)_test -t halt

.PHONY: repl
repl:
	$(PROLOG) -s $(FILENAME)