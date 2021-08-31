#!/usr/bin/env bash
# Plot each transaction confirmation time using gnuplot

set -e

[ $# -eq 1 ] || { echo "Expecting benchmark output directory as argument"; exit 1; }

DIR=$1

gnuplot <<EOF
set term pngcairo size 2048,1532
set output "${DIR}/results.png"
set xlabel "Time"
set ylabel "Confirmation delay (s)"
set ylabel "Tx confirmation time (s)"
set xtics out rotate by -80
set xdata time
set format x "%H:%M:%S"
set datafile separator ","
plot "${DIR}/results.csv" u (timecolumn(1,"%Y-%m-%d %H:%M:%S")):2 t 'Tx Confirmation over time'
EOF

echo "Created plot: ${DIR}/results.png"
