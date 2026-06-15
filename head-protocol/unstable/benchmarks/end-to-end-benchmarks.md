--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-15 15:18:41.217147761 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 549.6 |
| _P99_ | 573.2ms |
| _P95_ | 572.7ms |
| _P50_ | 555.3ms |
| _End-to-end TPS_ | 516.30 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2326.58 tx/s |
| _Per-snapshot TPS P95_ | 4648.76 tx/s |
| _Per-snapshot TPS max_ | 4913.58 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3223.0 |
| _P99_ | 3678.5ms |
| _P95_ | 3640.4ms |
| _P50_ | 3324.3ms |
| _End-to-end TPS_ | 244.18 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 603.20 tx/s |
| _Per-snapshot TPS P95_ | 1953.30 tx/s |
| _Per-snapshot TPS max_ | 2441.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
