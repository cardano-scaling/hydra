--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-07 09:08:49.515512351 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 5.333247443 |
| _P99_ | 22.481424969999992ms |
| _P95_ | 7.630632500000003ms |
| _P50_ | 4.5147595ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 40.019072227 |
| _P99_ | 62.57809256ms |
| _P95_ | 55.47581829999999ms |
| _P50_ | 38.9720515ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
