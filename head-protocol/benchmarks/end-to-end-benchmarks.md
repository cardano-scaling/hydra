--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-07-15 12:34:03.644295574 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 564.0 |
| _P99_ | 581.7ms |
| _P95_ | 581.4ms |
| _P50_ | 565.7ms |
| _End-to-end TPS_ | 510.14 tx/s |
| _Snapshots observed_ | 4 |
| _Peak sustained TPS_ | 300.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 2951.6 |
| _P99_ | 3263.5ms |
| _P95_ | 3232.9ms |
| _P50_ | 3020.5ms |
| _End-to-end TPS_ | 274.70 tx/s |
| _Snapshots observed_ | 10 |
| _Peak sustained TPS_ | 899.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
