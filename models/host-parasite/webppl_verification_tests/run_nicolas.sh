#!/bin/zsh

echo "\"algorithm\"\tz" > nicolas_results.tsv

webppl naive.wppl --compile naive.js
webppl nicolas.wppl --compile nicolas.js

for ((i=0; i<100; i++))
do
    echo -n "naive\t" >> nicolas_results.tsv
    node naive.js >> nicolas_results.tsv
    echo -n "nicolas\t" >> nicolas_results.tsv
    node nicolas.js >> nicolas_results.tsv
done

