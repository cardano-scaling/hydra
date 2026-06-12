--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-12 15:09:15.01454987 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 535.2 |
| _P99_ | 559.1ms |
| _P95_ | 559.0ms |
| _P50_ | 540.5ms |
| _End-to-end TPS_ | 531.16 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2362.40 tx/s |
| _Per-snapshot TPS P95_ | 4674.29 tx/s |
| _Per-snapshot TPS max_ | 4931.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2955.9 |
| _P99_ | 3393.2ms |
| _P95_ | 3392.3ms |
| _P50_ | 3060.0ms |
| _End-to-end TPS_ | 264.37 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 624.83 tx/s |
| _Per-snapshot TPS P95_ | 1738.13 tx/s |
| _Per-snapshot TPS max_ | 1879.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
