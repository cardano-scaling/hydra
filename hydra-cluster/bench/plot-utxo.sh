#!/usr/bin/env bash
# Plot evolution of UTXO set size over time

set -e

[ $# -eq 1 ] && [ -f $1 ] || {
    echo "Expecting path to benchmark results.csv as FILE argument"
    echo "Usage: $0 FILE"
    exit 1
}

RESULTS=$1
DIR=$(dirname ${RESULTS})
COUNT=$(cat ${RESULTS} | wc -l)
NAME=$(basename ${DIR})

gnuplot <<EOF
set title "UTXO Size over Time- ${NAME} - ${COUNT} txs"
set term pngcairo size 2048,1532
set output "${DIR}/utxo-size.png"
set xlabel "Time"
set xtics out rotate
set xdata time
set format x "%H:%M:%S"
set ylabel "# UTXO"
plot '${DIR}/utxo-time' u  (timecolumn(1,"%Y-%m-%dT%H:%M:%SZ")):2 w l t 'UTXO Set'
EOF

echo "Created plot: ${DIR}/utxo-size.png"
