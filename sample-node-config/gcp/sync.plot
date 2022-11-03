set y2range [0:1]
set y2tics 0.1
set xlabel "Time"
set logscale y 2
set term svg
set output 'sync_times.svg'
set term pdf
set output 'sync_times.pdf'
plot 'sync_times' u 1:($2 / 60) t 'block/s' w lines, \
     'sys_times.tsv' u 1:2 t 'CPU %' w lines axis x1y2, \
     'sys_times.tsv' u 1:3 t 'Mem (GB)' w lines axis x1y1, \
     'sys_times.tsv' u 1:($4 / 60) t 'Net (MB/s)' w lines axis x1y2, \
