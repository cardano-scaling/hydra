--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-15 09:20:50.236414903 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 543.6 |
| _P99_ | 566.9ms |
| _P95_ | 566.7ms |
| _P50_ | 548.3ms |
| _End-to-end TPS_ | 524.98 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2484.75 tx/s |
| _Per-snapshot TPS P95_ | 4749.57 tx/s |
| _Per-snapshot TPS max_ | 4993.79 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3157.3 |
| _P99_ | 3605.7ms |
| _P95_ | 3592.3ms |
| _P50_ | 3254.5ms |
| _End-to-end TPS_ | 249.11 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 612.06 tx/s |
| _Per-snapshot TPS P95_ | 1749.92 tx/s |
| _Per-snapshot TPS max_ | 2033.44 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
