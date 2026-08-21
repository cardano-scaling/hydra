--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-21 10:13:56.575740715 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 177.3 |
| _P99_ | 179.8ms |
| _P95_ | 179.5ms |
| _P50_ | 177.6ms |
| _Tx validation time p50 (ms)_ | 119.2 |
| _End-to-end TPS_ | 1632.04 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 16.32 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 969.7 |
| _P99_ | 1016.1ms |
| _P95_ | 1015.8ms |
| _P50_ | 983.1ms |
| _Tx validation time p50 (ms)_ | 496.4 |
| _End-to-end TPS_ | 883.19 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.94 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Avg. Confirmation Time (ms)_ | 1836.0 |
| _P99_ | 2274.8ms |
| _P95_ | 2274.7ms |
| _P50_ | 2269.3ms |
| _Tx validation time p50 (ms)_ | 307.3 |
| _End-to-end TPS_ | 263.30 tx/s |
| _Backlog drain time (s)_ | 2.3 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.32 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 206.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
