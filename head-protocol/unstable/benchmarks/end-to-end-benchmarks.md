--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-17 13:25:31.533850712 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 543.8 |
| _P99_ | 560.2ms |
| _P95_ | 559.9ms |
| _P50_ | 548.7ms |
| _End-to-end TPS_ | 532.82 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 3783.06 tx/s |
| _Per-snapshot TPS P95_ | 7350.24 tx/s |
| _Per-snapshot TPS max_ | 7808.55 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2949.0 |
| _P99_ | 3272.2ms |
| _P95_ | 3233.1ms |
| _P50_ | 3010.7ms |
| _End-to-end TPS_ | 274.27 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 864.43 tx/s |
| _Per-snapshot TPS P95_ | 2392.86 tx/s |
| _Per-snapshot TPS max_ | 2535.93 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
