#!/bin/sh

# TODO: Replace this with a B program
set -e

for f in test/*; do
    echo "Testing $f"
    cargo -q run --release -- -r $f
done
