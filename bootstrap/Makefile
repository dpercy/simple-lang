SHELL=/bin/bash -o pipefail

SOURCES  = $(wildcard *.sl)
OUTPUTS  = $(SOURCES:.sl=.mjs)
OUTPUTS2 = $(SOURCES:.sl=.mjs.bootstrapped)
DIFFS    = $(SOURCES:.sl=.diff)

NODE = node --experimental-modules --no-warnings

# Don't discard intermediate files
.SECONDARY:


.PHONY: default
default: $(DIFFS)


%.diff: %.mjs %.mjs.bootstrapped
	diff $^


# The first compilation (.mjs) depends on:
# - the source file itself, as the input
# - the Racket programs that eval compiler.sl
# - compiler.sl, and its dependencies
%.mjs: %.sl $(wildcard *.rkt) $(wildcard *.sl)
	racket test-boot.rkt < $*.sl > $*.mjs


# The second compilation (.mjs.bootstrapped) depends on:
# - the source file
# - the first compiler (.mjs)
# - any other .mjs files like bootstrap.mjs or primitives.mjs
# Note the second compilation does NOT depend on Racket
%.mjs.bootstrapped: %.sl $(wildcard *.mjs) $(OUTPUTS)
	$(NODE) bootstrap.mjs < $*.sl > $*.mjs.bootstrapped


clean:
	rm -rf $(OUTPUTS) $(OUTPUTS2)
