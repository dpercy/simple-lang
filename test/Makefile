SHELL=/bin/bash -o pipefail
DIFF = diff -U10
COLORDIFF = $(shell which -a colordiff cat 2>/dev/null | head -n 1)

NODE = node --experimental-modules --no-warnings

TEST_PROGRAMS = $(wildcard *.sl)
TEST_OUTPUTS = $(TEST_PROGRAMS:.sl=.out)
TEST_DIFFS = $(TEST_PROGRAMS:.sl=.diff)

# Don't automatically delete intermediate files.
.SECONDARY:


.PHONY: default
default: $(TEST_DIFFS)


.PHONY: %.diff
%.diff: %.out %.node-out
	$(DIFF) $*.out $*.node-out | $(COLORDIFF)


%.out:
	@printf >&2 '\033[31mYou need to implement %s\033[m\n' $@
	@false



.PRECIOUS: %.node-out
%.node-out: build/%.mjs diff/%
	$(NODE) $< >$@


diff/%: build/%.mjs build/%.mjs.bootstrapped
	$(DIFF) $^ | $(COLORDIFF)

# One way to compile a .sl file is by running compiler.sl on Racket.
# This depends on:
# - the input .sl file
# - the .rkt shims for running compiler.sl on Racket
# - the compiler itself: a set of .sl files in bootstrap/
build/%.mjs: %.sl $(wildcard ../bootstrap/*.rkt) $(wildcard ../boostrap/*.sl) build/primitives.mjs build/BigInteger.js
	racket  ../bootstrap/test-boot.rkt <$<  >$@

# Another way is to run compiler.mjs in node.
# This depends on:
# - the input .sl file
# - the compiler itself (a phony target with its own makefile)
build/%.mjs.bootstrapped: %.sl ../bootstrap/compiler.mjs build/primitives.mjs
	$(NODE) ../bootstrap/bootstrap.mjs <$<  >$@

build/primitives.mjs:
	mkdir -p build
	cp ../bootstrap/primitives.mjs build/

build/BigInteger.js:
	mkdir -p build
	cp ../bootstrap/BigInteger.js build/


../bootstrap/compiler.mjs:
	$(MAKE) -C ../bootstrap/


.PHONY: clean
clean:
	rm -rf build *.node-out
	$(MAKE) -C ../bootstrap/ clean
