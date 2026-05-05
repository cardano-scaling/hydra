--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-05-05 18:11:39.306837278 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.074463366 |
| _P99_ | 10.341283409999996ms |
| _P95_ | 7.48352415ms |
| _P50_ | 4.6291645ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 40.062463320 |
| _P99_ | 63.356291320000004ms |
| _P95_ | 55.60540779999999ms |
| _P50_ | 38.9413025ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
