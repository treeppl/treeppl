#!/bin/zsh

echo "\"algorithm\"\tz" > midpoint_results.tsv

webppl nomidpoint.wppl --compile --out nomidpoint.js
webppl midpoint.wppl --compile --out midpoint.js

for ((i=0; i<100; i++))
do
    echo -n "nomidpoint\t" >> midpoint_results.tsv
    node nomidpoint.js >> midpoint_results.tsv
    echo -n "midpoint\t" >> midpoint_results.tsv
    node midpoint.js >> midpoint_results.tsv
done

rm nomidpoint.js
rm midpoint.js

