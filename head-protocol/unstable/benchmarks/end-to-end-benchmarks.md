--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-06-15 12:42:33.441724786 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 550.5 |
| _P99_ | 575.2ms |
| _P95_ | 574.9ms |
| _P50_ | 556.0ms |
| _End-to-end TPS_ | 518.51 tx/s |
| _Snapshots observed_ | 4 |
| _Per-snapshot TPS P50_ | 2351.96 tx/s |
| _Per-snapshot TPS P95_ | 4617.97 tx/s |
| _Per-snapshot TPS max_ | 4864.38 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 3387.9 |
| _P99_ | 3872.7ms |
| _P95_ | 3855.8ms |
| _P50_ | 3499.2ms |
| _End-to-end TPS_ | 231.90 tx/s |
| _Snapshots observed_ | 10 |
| _Per-snapshot TPS P50_ | 547.85 tx/s |
| _Per-snapshot TPS P95_ | 1707.47 tx/s |
| _Per-snapshot TPS max_ | 1964.80 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
