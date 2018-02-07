SHELL=/bin/bash -o pipefail
DIFF = diff -U10

# If colordiff is installed, use colordiff --color=yes.
# Otherwise fall back to cat.
# --color=yes forces color output even when using gmake -j -O
COLORDIFF = $(shell if which -s colordiff ; then echo colordiff --color=yes ; else echo cat ; fi)

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
%.node-out: build/%.js ../bootstrap/rts.js diff/%
	node ../bootstrap/rts.js $< >$@


diff/%: build/%.js build/%.js.bootstrapped
	$(DIFF) $^ | $(COLORDIFF)

# One way to compile a .sl file is by running compiler.sl on Racket.
# This depends on:
# - the input .sl file
# - the .rkt shims for running compiler.sl on Racket
# - the compiler itself: a set of .sl files in bootstrap/
build/%.js: %.sl $(wildcard ../bootstrap/*.rkt) $(wildcard ../bootstrap/*.sl) build/primitives.js build/BigInteger.js
	racket  ../bootstrap/test-boot.rkt <$<  >$@

# Another way is to run compiler.js in node.
# This depends on:
# - the input .sl file
# - the compiler itself (a phony target with its own makefile)
# - the bootstrap script and runtime system
# TODO hacky that you have to cd over there...
build/%.js.bootstrapped: %.sl ../bootstrap/compiler.js ../bootstrap/bootstrap.js ../bootstrap/rts.js build/primitives.js
	(cd ../bootstrap && node bootstrap.js) <$<  >$@

build/primitives.js:
	mkdir -p build
	cp ../bootstrap/primitives.js build/

build/BigInteger.js:
	mkdir -p build
	cp ../bootstrap/BigInteger.js build/


../bootstrap/compiler.js:
	$(MAKE) -C ../bootstrap/


.PHONY: clean
clean:
	rm -rf build *.node-out
	$(MAKE) -C ../bootstrap/ clean
