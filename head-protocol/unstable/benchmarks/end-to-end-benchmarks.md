--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-18 12:50:20.866905947 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 495.3 |
| _P99_ | 508.7ms |
| _P95_ | 508.5ms |
| _P50_ | 499.3ms |
| _End-to-end TPS_ | 585.10 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 4781.66 tx/s |
| _Per-snapshot TPS P95_ | 8358.52 tx/s |
| _Per-snapshot TPS max_ | 8852.99 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2595.1 |
| _P99_ | 2826.2ms |
| _P95_ | 2818.7ms |
| _P50_ | 2636.6ms |
| _End-to-end TPS_ | 315.72 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 1211.37 tx/s |
| _Per-snapshot TPS P95_ | 2852.78 tx/s |
| _Per-snapshot TPS max_ | 3326.89 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
