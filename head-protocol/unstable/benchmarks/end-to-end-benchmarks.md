--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-18 09:57:34.849196353 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 495.6 |
| _P99_ | 510.6ms |
| _P95_ | 506.3ms |
| _P50_ | 500.6ms |
| _End-to-end TPS_ | 580.93 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 3957.06 tx/s |
| _Per-snapshot TPS P95_ | 7909.17 tx/s |
| _Per-snapshot TPS max_ | 8382.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2713.5 |
| _P99_ | 3002.0ms |
| _P95_ | 2970.2ms |
| _P50_ | 2767.2ms |
| _End-to-end TPS_ | 298.69 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 1019.14 tx/s |
| _Per-snapshot TPS P95_ | 2366.47 tx/s |
| _Per-snapshot TPS max_ | 2591.80 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
