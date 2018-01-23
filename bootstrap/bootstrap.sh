#!/bin/bash
set -ex

modules=(
    bool
    int
    string
    list
    compiler
)

# 1. cross compile the compiler and its deps to JS
for name in ${modules[@]}
do
    racket test-boot.rkt < $name.sl > $name.mjs &
done
wait

# 2. test the compiler-in-JS on some inputs to see if
#    it has the same behavior as the compiler-in-Racket.
for name in ${modules[@]}
do
    node --experimental-modules bootstrap.mjs < $name.sl > ${name}2.mjs &
done
wait

for name in ${modules[@]}
do
    diff $name.mjs ${name}2.mjs
done
echo 'yay!'
