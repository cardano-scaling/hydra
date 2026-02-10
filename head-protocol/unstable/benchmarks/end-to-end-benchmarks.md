--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-02-10 15:43:59.827300055 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.382082543 |
| _P99_ | 11.554824759999994ms |
| _P95_ | 6.52383525ms |
| _P50_ | 5.086159500000001ms |
| _Number of Invalid txs_ | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 38.288728034 |
| _P99_ | 67.98189934999999ms |
| _P95_ | 57.899578899999995ms |
| _P50_ | 36.35079ms |
| _Number of Invalid txs_ | 0 |
      
