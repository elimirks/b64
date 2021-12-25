#!/bin/bash

# Generates a large amount of garbage B code, for benchmarking

GARBAGE_COUNT=10000

for i in $(seq $GARBAGE_COUNT); do
    sed "s/func/func_${i}_/g" base_garbage.b > "garb_${i}.b"
done

for i in `seq 10000`; do
        echo "#import \"garb_${i}.b\";"
done > all_garbage.b

# Run b64 against "all_garbage.b" to benchmark
echo "main() {}" >> all_garbage.b
