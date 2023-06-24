#!/bin/zsh

echo "\"algorithm\"\tz" > propose_results.tsv

webppl nopropose.wppl --compile --out nopropose.js
webppl propose.wppl --compile --out propose.js

for ((i=0; i<100; i++))
do
    echo -n "nopropose\t" >> propose_results.tsv
    node nopropose.js >> propose_results.tsv
    echo -n "propose\t" >> propose_results.tsv
    node propose.js >> propose_results.tsv
done

rm nopropose.js
rm propose.js

