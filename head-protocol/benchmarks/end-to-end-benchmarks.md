--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-12 15:36:11.235253062 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 565.1 |
| _P99_ | 591.5ms |
| _P95_ | 591.1ms |
| _P50_ | 570.5ms |
| _End-to-end TPS_ | 502.65 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2192.32 tx/s |
| _Per-snapshot TPS P95_ | 4176.15 tx/s |
| _Per-snapshot TPS max_ | 4374.89 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3176.4 |
| _P99_ | 3632.3ms |
| _P95_ | 3611.2ms |
| _P50_ | 3281.9ms |
| _End-to-end TPS_ | 247.06 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 583.44 tx/s |
| _Per-snapshot TPS P95_ | 1949.02 tx/s |
| _Per-snapshot TPS max_ | 2348.47 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
