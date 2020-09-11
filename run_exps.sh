#!/bin/bash

limit=0
for i in {0..100} ; do
    echo "${i}";
    timeout 20m python3 main_best.py --binary_idx=${i} --output=$1 --dataset $2  &
    let limit=limit+1;
    if (( limit == 10 )); then
        echo "wating";
        wait;
        let limit=0;
    fi;
done;

wait
