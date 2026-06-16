--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-16 13:25:38.260259619 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 560.5 |
| _P99_ | 590.1ms |
| _P95_ | 590.0ms |
| _P50_ | 566.6ms |
| _End-to-end TPS_ | 506.14 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2277.22 tx/s |
| _Per-snapshot TPS P95_ | 3915.25 tx/s |
| _Per-snapshot TPS max_ | 4082.94 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3232.4 |
| _P99_ | 3703.6ms |
| _P95_ | 3702.9ms |
| _P50_ | 3324.7ms |
| _End-to-end TPS_ | 242.33 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 595.20 tx/s |
| _Per-snapshot TPS P95_ | 1728.49 tx/s |
| _Per-snapshot TPS max_ | 1953.73 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
