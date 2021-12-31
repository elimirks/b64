#!/bin/sh

# TODO: Replace this with a B program

for f in test/*; do
    cargo -q run --release -- -r $f
done
