SHELL=/bin/bash -o pipefail

SOURCES  = $(wildcard *.sl)
OUTPUTS  = $(SOURCES:.sl=.js)
OUTPUTS2 = $(SOURCES:.sl=.js.bootstrapped)
DIFFS    = $(SOURCES:.sl=.diff)

NODE = node

# Don't discard intermediate files
.SECONDARY:


.PHONY: default stage1
default: $(DIFFS)
stage1: $(OUTPUTS)


%.diff: %.js %.js.bootstrapped
	diff $^


# The first compilation (.js) depends on:
# - the source file itself, as the input
# - the Racket programs that eval compiler.sl
# - compiler.sl, and its dependencies
%.js: %.sl $(wildcard *.rkt) $(wildcard *.sl)
	racket test-boot.rkt < $*.sl > $*.js


# The second compilation (.js.bootstrapped) depends on:
# - the source file
# - the first compiler (.js)
# - any other .js files like bootstrap.js or primitives.js
# Note the second compilation does NOT depend on Racket
%.js.bootstrapped: %.sl $(wildcard *.js) $(OUTPUTS)
	$(NODE) bootstrap.js < $*.sl > $*.js.bootstrapped


clean:
	rm -rf $(OUTPUTS) $(OUTPUTS2)
