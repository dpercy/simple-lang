

default: test/numbers

# TODO convince make not to delete intermediate files...

# build1 contains .mjs files compiled by Racket
build1:
	mkdir $@
	cd $@ && ln -s ../../bootstrap/primitives.mjs
build1/%.mjs: %.sl | build1
	racket ../bootstrap/test-boot.rkt < $< > $@

# build2 contains .mjs files compiled by compiler.mjs
build2:
	mkdir $@
	cd $@ && ln -s ../../bootstrap/primitives.mjs
build2/%.mjs: %.sl | build2
	node --experimental-modules ../bootstrap/bootstrap.mjs ../bootstrap/compiler.mjs < $< > $@

# diff/%.mjs is a phony target representing that you should diff those two files
# TODO do I need to mark this as phony?
diff/%.mjs: build1/%.mjs build2/%.mjs
	diff $^


# to get the output of a .sl file,
# you first compile it both ways and confirm they match,
# then you run the compiled file (either one) in node.
%.actual.out: build1/%.mjs build2/%.mjs diff/%.mjs
	node --experimental-modules $< > $@

# test/% is a phony target meaning: does this test pass?
# TODO could make this a real file that gets aggregated into a report
test/%: %.expected.out %.actual.out
	diff $^

.PHONY: clean
clean:
	rm -rf build1 build2 *.actual.out
