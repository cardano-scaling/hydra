--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-28 15:41:50.967318417 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 6.787891873 |
| _P99_ | 12.111950579999057ms |
| _P95_ | 7.0856315500000004ms |
| _P50_ | 4.447303ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 37.743294696 |
| _P99_ | 59.48638528ms |
| _P95_ | 50.95530684999999ms |
| _P50_ | 37.0356715ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
