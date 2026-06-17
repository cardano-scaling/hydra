--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-17 14:48:18.111868477 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 505.2 |
| _P99_ | 519.3ms |
| _P95_ | 519.0ms |
| _P50_ | 508.5ms |
| _End-to-end TPS_ | 569.86 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 3912.29 tx/s |
| _Per-snapshot TPS P95_ | 7493.00 tx/s |
| _Per-snapshot TPS max_ | 7920.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2713.5 |
| _P99_ | 2992.8ms |
| _P95_ | 2972.1ms |
| _P50_ | 2769.9ms |
| _End-to-end TPS_ | 299.33 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 1027.46 tx/s |
| _Per-snapshot TPS P95_ | 2998.80 tx/s |
| _Per-snapshot TPS max_ | 3634.45 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
