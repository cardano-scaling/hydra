--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-02-17 15:48:47.827232217 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.717290133 |
| _P99_ | 8.866660649999979ms |
| _P95_ | 7.128968300000001ms |
| _P50_ | 5.4635955ms |
| _Number of Invalid txs_ | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 35.037140164 |
| _P99_ | 60.95244456999999ms |
| _P95_ | 45.12131134999999ms |
| _P50_ | 33.828933500000005ms |
| _Number of Invalid txs_ | 0 |
      
