--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-21 11:17:37.090739561 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 140.2 |
| _P99_ | 142.0ms |
| _P95_ | 141.8ms |
| _P50_ | 140.6ms |
| _Tx validation time p50 (ms)_ | 95.5 |
| _End-to-end TPS_ | 2072.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 20.72 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 755.8 |
| _P99_ | 785.3ms |
| _P95_ | 784.7ms |
| _P50_ | 766.2ms |
| _Tx validation time p50 (ms)_ | 392.6 |
| _End-to-end TPS_ | 1137.70 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.79 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Avg. Confirmation Time (ms)_ | 1438.3 |
| _P99_ | 1780.5ms |
| _P95_ | 1780.3ms |
| _P50_ | 1774.1ms |
| _Tx validation time p50 (ms)_ | 253.1 |
| _End-to-end TPS_ | 336.21 tx/s |
| _Backlog drain time (s)_ | 1.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.68 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 205.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
