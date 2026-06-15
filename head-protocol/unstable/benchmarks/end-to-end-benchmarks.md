--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-15 09:03:24.243974947 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 472.8 |
| _P99_ | 495.8ms |
| _P95_ | 495.5ms |
| _P50_ | 477.6ms |
| _End-to-end TPS_ | 599.75 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2630.84 tx/s |
| _Per-snapshot TPS P95_ | 4873.94 tx/s |
| _Per-snapshot TPS max_ | 5126.52 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2603.5 |
| _P99_ | 3014.8ms |
| _P95_ | 3005.0ms |
| _P50_ | 2692.5ms |
| _End-to-end TPS_ | 297.56 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 657.96 tx/s |
| _Per-snapshot TPS P95_ | 2149.32 tx/s |
| _Per-snapshot TPS max_ | 2628.09 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
