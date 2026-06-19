--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-19 14:01:18.538739852 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 427.5 |
| _P99_ | 438.0ms |
| _P95_ | 437.7ms |
| _P50_ | 429.7ms |
| _End-to-end TPS_ | 680.10 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 5968.91 tx/s |
| _Per-snapshot TPS P95_ | 10197.69 tx/s |
| _Per-snapshot TPS max_ | 10646.57 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2580.1 |
| _P99_ | 2774.7ms |
| _P95_ | 2766.4ms |
| _P50_ | 2615.9ms |
| _End-to-end TPS_ | 323.39 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 1499.17 tx/s |
| _Per-snapshot TPS P95_ | 3996.16 tx/s |
| _Per-snapshot TPS max_ | 4446.14 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
