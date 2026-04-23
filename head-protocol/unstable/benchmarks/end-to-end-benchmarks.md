--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-04-23 12:11:48.155366878 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 23.601398153 |
| _P99_ | 461.17444706ms |
| _P95_ | 93.93765010000001ms |
| _P50_ | 4.6594365ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 119.055140901 |
| _P99_ | 739.8664429199991ms |
| _P95_ | 424.35270065ms |
| _P50_ | 42.572173500000005ms |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      
