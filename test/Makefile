


default: diff/numbers

DIFF = diff -U10
COLORDIFF = $(shell which -a colordiff cat 2>/dev/null | head -n 1)


# Don't automatically delete intermediate files.
.SECONDARY:


diff/%: %.expected.out %.racket.out %.node.out
	$(DIFF) $*.expected.out $*.racket.out | $(COLORDIFF)
	$(DIFF) $*.expected.out $*.node.out | $(COLORDIFF)


# There are 2 ways to evaluate a .sl program:
#  1. run it in Racket directly
#  2. compile it using compiler.sl
#    2a. run compiler.sl in Racket directly
#    2b. run the self-compiled version of compiler.sl
# I'm calling this 2 ways rather than 3 because 2a and 2b
# should produce identical .mjs files.

.PRECIOUS: %.racket.out
%.racket.out: %.sl
	racket $< >$@

.PRECIOUS: %.node.out
%.node.out: build/%.mjs
	node --experimental-modules --no-warnings $<  >$@

# TODO there are undeclared dependencies on compiler.mjs...
#   -  can solve by turning bootstrap.sh into a makefile

# To create a .mjs file,
# you run the .sl file through both compilers and verify that they match.
build/%.mjs: %.sl | build
	racket ../bootstrap/test-boot.rkt <$<  >$@
	node --experimental-modules --no-warnings ../bootstrap/bootstrap.mjs ../bootstrap/compiler.mjs <$<  >$@.bootstrapped
	$(DIFF) $@ $@.bootstrapped | $(COLORDIFF)

# The build directory should exist and contain a link to primitives.mjs
build:
	mkdir $@
	cd $@ && ln -s ../../bootstrap/primitives.mjs
