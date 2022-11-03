set logscale y 2
set term svg
set output 'sync_times.svg'
set term pdf
set output 'sync_times.pdf'
plot 'sync_times' u 3:($2 / 10) t 'block/s' w lines
