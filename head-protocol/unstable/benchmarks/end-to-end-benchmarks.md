--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-03-27 16:25:58.633252846 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 9.832160240 |
| _P99_ | 185.42992725999983ms |
| _P95_ | 8.760202350000002ms |
| _P50_ | 5.5287585ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 46.669082862 |
| _P99_ | 68.31703494ms |
| _P95_ | 60.807925199999985ms |
| _P50_ | 46.002742ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
