--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-05-06 12:44:45.814251428 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 10.158997963 |
| _P99_ | 57.3478701299982ms |
| _P95_ | 10.581867050000001ms |
| _P50_ | 5.535225499999999ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 43.957804944 |
| _P99_ | 65.93476949ms |
| _P95_ | 57.43808739999999ms |
| _P50_ | 43.615891500000004ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
