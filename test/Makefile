


default: diff/numbers


diff/%: %.expected.out %.racket.out %.node.out
	diff $*.expected.out $*.racket.out
	diff $*.expected.out $*.node.out


# There are 2 ways to evaluate a .sl program:
#  1. run it in Racket directly
#  2. compile it using compiler.sl
#    2a. run compiler.sl in Racket directly
#    2b. run the self-compiled version of compiler.sl
# I'm calling this 2 ways rather than 3 because 2a and 2b
# should produce identical .mjs files.


%.racket.out: %.sl
	racket $< >$@

%.node.out: build/%.mjs
	node --experimental-modules --no-warnings $<  >$@


# To create a .mjs file,
# you run the .sl file through both compilers and verify that they match.
build/%.mjs: %.sl | build
	@# compile through racket to create the target
	racket ../bootstrap/test-boot.rkt <$<  >$@
	@# compile through node as an integrity check
	node --experimental-modules --no-warnings ../bootstrap/bootstrap.mjs ../bootstrap/compiler.mjs <$<  >$@.bootstrapped
	diff $@ $@.bootstrapped

# The build directory should exist and contain a link to primitives.mjs
build:
	mkdir $@
	cd $@ && ln -s ../../bootstrap/primitives.mjs
