--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-02 14:15:45.397661135 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 9.292907380 |
| _P99_ | 16.161173439999256ms |
| _P95_ | 7.665669900000003ms |
| _P50_ | 4.7988175ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 43.325017871 |
| _P99_ | 68.41582564999999ms |
| _P95_ | 56.95219454999999ms |
| _P50_ | 42.005255500000004ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
