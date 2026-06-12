--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-12 16:30:16.797114704 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 474.7 |
| _P99_ | 498.6ms |
| _P95_ | 498.4ms |
| _P50_ | 478.6ms |
| _End-to-end TPS_ | 596.11 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2644.85 tx/s |
| _Per-snapshot TPS P95_ | 4511.64 tx/s |
| _Per-snapshot TPS max_ | 4712.47 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2663.1 |
| _P99_ | 3073.6ms |
| _P95_ | 3073.1ms |
| _P50_ | 2745.2ms |
| _End-to-end TPS_ | 291.66 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 672.32 tx/s |
| _Per-snapshot TPS P95_ | 1994.91 tx/s |
| _Per-snapshot TPS max_ | 2347.66 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
