--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-23 08:16:44.618709543 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 190.5 |
| _P99_ | 193.7ms |
| _P95_ | 193.5ms |
| _P50_ | 191.6ms |
| _Tx validation time p50 (ms)_ | 85.6 |
| _End-to-end TPS_ | 1544.31 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.30 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 949.5 |
| _P99_ | 1026.3ms |
| _P95_ | 1025.9ms |
| _P50_ | 913.2ms |
| _Tx validation time p50 (ms)_ | 379.8 |
| _End-to-end TPS_ | 872.99 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.91 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 818.8 |
| _P99_ | 926.3ms |
| _P95_ | 925.8ms |
| _P50_ | 777.0ms |
| _Tx validation time p50 (ms)_ | 207.5 |
| _End-to-end TPS_ | 636.12 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.18 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
