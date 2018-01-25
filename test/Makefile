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


# TODO try a better diff3 like "diffuse" ?
.PHONY: %.diff
%.diff: %.out %.racket-out %.node-out
	$(DIFF) $*.out $*.racket-out | $(COLORDIFF)
	$(DIFF) $*.out $*.node-out | $(COLORDIFF)


%.out:
	@printf >&2 '\033[31mYou need to implement %s\033[m\n' $@
	@false


# There are 2 ways to evaluate a .sl program:
#  1. run it in Racket directly
#  2. compile it using compiler.sl
#    2a. run compiler.sl in Racket directly
#    2b. run the self-compiled version of compiler.sl
# I'm calling this 2 ways rather than 3 because 2a and 2b
# should produce identical .mjs files.

.PRECIOUS: %.racket-out
%.racket-out: %.sl $(wildcard ../bootstrap/*.rkt)
	racket $*.sl >$*.racket-out

.PRECIOUS: %.node-out
%.node-out: build/%.mjs
	$(NODE) build/$*.mjs >$*.node-out


# To create a .mjs file,
# you run the .sl file through both compilers and verify that they match.
build/%.mjs: %.sl $(wildcard ../bootstrap/*.rkt) ../bootstrap/compiler.mjs    | build
	racket  ../bootstrap/test-boot.rkt <$*.sl  >build/$*.mjs
	$(NODE) ../bootstrap/bootstrap.mjs <$*.sl  >build/$*.mjs.bootstrapped
	$(DIFF) $@ $@.bootstrapped | $(COLORDIFF)

# The build directory should exist and contain a link to primitives.mjs
build:
	mkdir $@
	cd $@ && ln -s ../../bootstrap/primitives.mjs


.PHONY: ../bootstrap/compiler.mjs
../bootstrap/compiler.mjs:
	$(MAKE) -C ../bootstrap/


.PHONY: clean
clean:
	rm -rf build *.racket-out *.node-out
	$(MAKE) -C ../bootstrap/ clean
