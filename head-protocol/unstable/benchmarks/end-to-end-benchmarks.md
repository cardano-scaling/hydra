--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-01-08 19:07:02.521734169 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.359124570 |
| _P99_ | 11.67203496ms |
| _P95_ | 6.56915795ms |
| _P50_ | 5.105329ms |
| _Number of Invalid txs_ | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 37.855480905 |
| _P99_ | 65.18649852999998ms |
| _P95_ | 57.1203411ms |
| _P50_ | 35.684518ms |
| _Number of Invalid txs_ | 0 |
      
