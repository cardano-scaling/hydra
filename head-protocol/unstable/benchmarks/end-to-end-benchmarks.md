--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-28 12:09:18.7154996 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 18.092257683 |
| _P99_ | 633.1625117299972ms |
| _P95_ | 8.33253785000002ms |
| _P50_ | 4.352357ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 43.535635664 |
| _P99_ | 180.70351979999998ms |
| _P95_ | 86.23070509999995ms |
| _P50_ | 36.478049ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
