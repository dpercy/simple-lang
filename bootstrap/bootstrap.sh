#!/bin/bash
set -ex

for name in bool int string list compiler
do
    racket test-boot.rkt < $name.sl > $name.mjs &
done

wait
