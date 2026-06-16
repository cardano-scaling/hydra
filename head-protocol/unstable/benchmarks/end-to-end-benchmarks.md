--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-16 10:20:09.016950345 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 538.1 |
| _P99_ | 562.7ms |
| _P95_ | 562.4ms |
| _P50_ | 544.2ms |
| _End-to-end TPS_ | 528.18 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2286.35 tx/s |
| _Per-snapshot TPS P95_ | 4636.81 tx/s |
| _Per-snapshot TPS max_ | 4898.21 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2963.2 |
| _P99_ | 3404.0ms |
| _P95_ | 3379.3ms |
| _P50_ | 3056.6ms |
| _End-to-end TPS_ | 263.49 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 586.01 tx/s |
| _Per-snapshot TPS P95_ | 1897.89 tx/s |
| _Per-snapshot TPS max_ | 2126.92 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
