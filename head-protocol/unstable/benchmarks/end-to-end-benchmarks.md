--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-07 13:13:21.714629216 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 220.7 |
| _P99_ | 233.3ms |
| _P95_ | 233.0ms |
| _P50_ | 217.2ms |
| _Tx validation time p50 (ms)_ | 125.7 |
| _End-to-end TPS_ | 1262.20 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 12.62 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1017.1 |
| _P99_ | 1087.2ms |
| _P95_ | 1086.9ms |
| _P50_ | 1003.8ms |
| _Tx validation time p50 (ms)_ | 465.7 |
| _End-to-end TPS_ | 823.74 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.75 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 969.4 |
| _P99_ | 1089.9ms |
| _P95_ | 1089.6ms |
| _P50_ | 1079.0ms |
| _Tx validation time p50 (ms)_ | 333.5 |
| _End-to-end TPS_ | 545.57 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.73 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
