#!/usr/bin/env bash
set -euo pipefail

tppl_exe=$1
data_file=$2

response_file=`mktemp`

$tppl_exe $data_file | tee $response_file

# Check that for an arithmetic progression of temperatures the log
# potential is an arithmetic progression too

# Extract only numeric response values; a blank response() becomes an
# empty line
awk '
  /^response\(/ {
    if ($0 == "response()") {
      print ""
    } else {
      match($0, /response\(([^)]+)\)/, a)
      print a[1]
    }
  }
' "$response_file" |
awk '
  # Process blocks separated by empty lines
  NF == 0 {
    check_block()
    delete vals
    n = 0
    next
  }
  {
    vals[n++] = $1
  }
  END {
    check_block()
  }

  function abs(x) {
    return x < 0 ? -x : x
  }

  function check_block(   d, i) {
    if (n < 2) return

    d = vals[1] - vals[0]
    for (i = 2; i < n; i++) {
      if (abs(vals[i] - vals[i-1] - d) > 1e-7) {
        printf "FAIL: not an arithmetic progression: "
        for (i = 0; i < n; i++) printf "%s ", vals[i]
        print ""
        exit 1
      }
    }
  }
'

rm $response_file
