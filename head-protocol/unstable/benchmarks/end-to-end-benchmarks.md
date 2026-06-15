--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-15 15:03:09.433551594 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 597.5 |
| _P99_ | 623.9ms |
| _P95_ | 623.9ms |
| _P50_ | 603.7ms |
| _End-to-end TPS_ | 478.49 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2241.94 tx/s |
| _Per-snapshot TPS P95_ | 4440.67 tx/s |
| _Per-snapshot TPS max_ | 4699.42 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3071.8 |
| _P99_ | 3528.1ms |
| _P95_ | 3516.6ms |
| _P50_ | 3168.5ms |
| _End-to-end TPS_ | 254.61 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 557.40 tx/s |
| _Per-snapshot TPS P95_ | 1942.22 tx/s |
| _Per-snapshot TPS max_ | 2349.97 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
