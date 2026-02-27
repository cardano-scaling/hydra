--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-02-27 14:19:08.821977302 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 6.426214696 |
| _P99_ | 11.304622929999997ms |
| _P95_ | 7.6658754500000015ms |
| _P50_ | 5.6820645ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 34.351241595 |
| _P99_ | 51.79889095ms |
| _P95_ | 44.577488550000005ms |
| _P50_ | 33.213183ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
