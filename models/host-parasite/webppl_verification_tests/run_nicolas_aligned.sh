#!/bin/zsh

echo "\"algorithm\"\tz" > nicolas_results.tsv

webppl naive.wppl --compile --out naive.js
webppl nicolas.wppl --compile --out nicolas.js
webppl nicolas_aligned.wppl --compile --out nicolas_aligned.js

for ((i=0; i<100; i++))
do
    echo -n "naive\t" >> nicolas_results.tsv
    node naive.js >> nicolas_results.tsv
    echo -n "nicolas\t" >> nicolas_results.tsv
    node nicolas.js >> nicolas_results.tsv
    echo -n "nicolas_aligned\t" >> nicolas_results.tsv
    node nicolas_aligned.js >> nicolas_results.tsv
done

