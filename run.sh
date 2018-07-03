#!/bin/bash
set -ex

./node_modules/.bin/pegjs  -o grammar.generated.js grammar.pegjs

# wtf
rm -rf .cache/

./node_modules/.bin/parcel serve index.html
