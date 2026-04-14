--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-14 15:10:41.160319472 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 10.140831146 |
| _P99_ | 53.42258884999777ms |
| _P95_ | 8.508512750000005ms |
| _P50_ | 5.4262215000000005ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 43.747169717 |
| _P99_ | 67.41425174999999ms |
| _P95_ | 59.947869999999995ms |
| _P50_ | 42.4187715ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
