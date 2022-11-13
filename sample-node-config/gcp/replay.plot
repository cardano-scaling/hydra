set xdata time
set xrange [1667980240:1667990312]
set timefmt "%s"
set format x "%H:%M"
set xlabel "Time"
set xtics rotate by -90
set y2label "MB/s"
set y2tics 0.1
set term svg
set output 'sync_replay_times.svg'
set term pdf
set output 'sync_replay_times.pdf'
plot 'sys_replay_times.tsv' u 1:2 t 'CPU (%)' w lines, \
     'sys_replay_times.tsv' u 1:3 t 'Mem (%)' w lines, \
     'sys_replay_times.tsv' u 1:($5 / 60) t 'Disk (MB/s)' w lines axis x1y2
