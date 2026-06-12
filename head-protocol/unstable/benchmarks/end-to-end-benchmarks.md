--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-12 14:43:34.123964801 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 539.1 |
| _P99_ | 562.9ms |
| _P95_ | 562.8ms |
| _P50_ | 543.9ms |
| _End-to-end TPS_ | 527.57 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2335.92 tx/s |
| _Per-snapshot TPS P95_ | 4731.69 tx/s |
| _Per-snapshot TPS max_ | 4992.72 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3014.3 |
| _P99_ | 3456.5ms |
| _P95_ | 3454.4ms |
| _P50_ | 3107.3ms |
| _End-to-end TPS_ | 259.10 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 588.47 tx/s |
| _Per-snapshot TPS P95_ | 1950.20 tx/s |
| _Per-snapshot TPS max_ | 2351.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
