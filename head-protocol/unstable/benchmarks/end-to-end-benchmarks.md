--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-15 08:42:53.42106732 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 9.847276106 |
| _P99_ | 146.1234361599995ms |
| _P95_ | 9.256447400000004ms |
| _P50_ | 5.27653ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 44.665451644 |
| _P99_ | 66.31494769999999ms |
| _P95_ | 58.297526850000004ms |
| _P50_ | 44.1742025ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
