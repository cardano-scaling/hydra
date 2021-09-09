#!/usr/bin/env bash
# Plot each transaction confirmation time using gnuplot

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
set title "Hydra Load Run - ${NAME} - ${COUNT} txs"
set term pngcairo size 2048,1532
set output "${DIR}/results.png"
set xlabel "Time"
set ylabel "Tx confirmation time (s)"
set xtics out rotate
set xdata time
set format x "%H:%M:%S"
set ytics nomirror
set y2tics 0, 10
set y2range [0:]
set y2label "Tx submitted/time interval (5s)"
set datafile separator ","
plot "${RESULTS}" u (timecolumn(1,"%Y-%m-%d %H:%M:%S")):2 t'Tx Confirmation over time' axis x1y1, \
  "${RESULTS}" u (timecolumn(1, "%Y-%m-%d %H:%M:%S UTC")):(1.0) bins binwidth=5 w histeps axis x1y2 t '# Tx submitted'
EOF

echo "Created plot: ${DIR}/results.png"
