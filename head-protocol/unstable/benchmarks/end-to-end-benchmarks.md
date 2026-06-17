--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-17 16:08:45.228390077 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 492.0 |
| _P99_ | 505.1ms |
| _P95_ | 504.8ms |
| _P50_ | 495.1ms |
| _End-to-end TPS_ | 587.38 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 4613.15 tx/s |
| _Per-snapshot TPS P95_ | 8208.60 tx/s |
| _Per-snapshot TPS max_ | 8684.97 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2773.2 |
| _P99_ | 3037.0ms |
| _P95_ | 3017.8ms |
| _P50_ | 2811.5ms |
| _End-to-end TPS_ | 295.66 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 1042.37 tx/s |
| _Per-snapshot TPS P95_ | 2808.27 tx/s |
| _Per-snapshot TPS max_ | 3116.67 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
