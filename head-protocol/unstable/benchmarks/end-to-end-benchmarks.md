--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-21 09:37:57.952703514 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 4.875525626 |
| _P99_ | 11.268884729999998ms |
| _P95_ | 7.183176400000002ms |
| _P50_ | 4.4407095000000005ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 39.884533528 |
| _P99_ | 61.689724319999996ms |
| _P95_ | 54.7080301ms |
| _P50_ | 39.38065ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
