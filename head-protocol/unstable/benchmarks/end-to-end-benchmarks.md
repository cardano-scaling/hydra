--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-05 16:04:59.293732433 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 6.407704330 |
| _P99_ | 10.442069969999999ms |
| _P95_ | 8.468154750000002ms |
| _P50_ | 6.021511500000001ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 40.312046312 |
| _P99_ | 59.885564239999994ms |
| _P95_ | 52.330577349999984ms |
| _P50_ | 39.292862ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
