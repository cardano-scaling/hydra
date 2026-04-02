--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-02 12:58:32.51792548 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.538415780 |
| _P99_ | 10.115111859999997ms |
| _P95_ | 7.58693135ms |
| _P50_ | 5.1954555ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 45.222052587 |
| _P99_ | 70.54068638ms |
| _P95_ | 60.668551949999994ms |
| _P50_ | 44.176843000000005ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
