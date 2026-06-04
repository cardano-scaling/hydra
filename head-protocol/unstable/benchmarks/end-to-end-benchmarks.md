--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-04 08:49:35.496348567 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 6.802230313 |
| _P99_ | 10.641627269999988ms |
| _P95_ | 9.089554300000001ms |
| _P50_ | 6.4312059999999995ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 41.712015797 |
| _P99_ | 64.47401049ms |
| _P95_ | 53.61862225000001ms |
| _P50_ | 40.770084499999996ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
