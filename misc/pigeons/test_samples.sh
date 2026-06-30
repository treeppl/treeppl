#!/usr/bin/env bash
set -euo pipefail

commands_file="$1"
samples_file="$2"

call_count=$(grep -c "call_sampler!(1.0)" "$commands_file" | tr -d ' ')
expected_count=$(("$call_count" * 2 + 1))
sample_count=$(wc -l < "$samples_file")
if [[ "$expected_count" -ne "$sample_count" ]]; then
    echo "FAIL: wrong number of samples, expected $expected_count, found $sample_count"
    echo "      in $samples_file"
    exit 1
fi
