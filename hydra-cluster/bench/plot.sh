#!/usr/bin/env bash
# Plot interesting data from a benchmark run
# - transaction confirmation time
# - system level metrics

set -e

[ $# -eq 1 ] && [ -d $1 ] || {
    echo "Expecting path to benchmark directory results as DIR argument"
    echo "Usage: $0 DIR"
    exit 1
}

DIR=$1
RESULTS=${DIR}/results.csv
SYSTEM=${DIR}/system.csv
COUNT=$(tail -n +2 ${RESULTS} | wc -l)
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
set y2label "Submitted Tx / s"
set datafile separator ","
plot "${RESULTS}" u (timecolumn(1,"%Y-%m-%d %H:%M:%S")):3 t 'Tx Confirmation over time' w histeps axis x1y1, \
     "${RESULTS}" u (timecolumn(1,"%Y-%m-%d %H:%M:%S")):2 t 'Tx Validation time' w histeps axis x1y1, \
     "${RESULTS}" u (timecolumn(1, "%Y-%m-%d %H:%M:%S UTC")):4 w histeps axis x1y2 t 'Submitted Tx/s', \
     "${SYSTEM}" u (timecolumn(1, "%Y-%m-%d %H:%M:%S UTC")):2 w histeps axis x1y1 t 'CPU %'
EOF

echo "Created plot: ${DIR}/results.png"
