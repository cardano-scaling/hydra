--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-02-09 13:08:46.627973455 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.657586163 |
| _P99_ | 12.954313859999987ms |
| _P95_ | 7.5725701500000016ms |
| _P50_ | 5.2344225ms |
| _Number of Invalid txs_ | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 33.703150903 |
| _P99_ | 51.574949969999984ms |
| _P95_ | 45.51610384999999ms |
| _P50_ | 32.415084ms |
| _Number of Invalid txs_ | 0 |
      
