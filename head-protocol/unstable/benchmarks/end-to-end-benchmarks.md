--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-19 17:52:08.486179375 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 181.8 |
| _P99_ | 187.6ms |
| _P95_ | 187.5ms |
| _P50_ | 181.3ms |
| _Tx validation time p50 (ms)_ | 112.9 |
| _End-to-end TPS_ | 1549.83 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 15.50 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 1032.0 |
| _P99_ | 1081.4ms |
| _P95_ | 1080.2ms |
| _P50_ | 1047.6ms |
| _Tx validation time p50 (ms)_ | 476.1 |
| _End-to-end TPS_ | 826.69 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.76 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 147.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Avg. Confirmation Time (ms)_ | 1940.9 |
| _P99_ | 2396.9ms |
| _P95_ | 2396.3ms |
| _P50_ | 2385.1ms |
| _Tx validation time p50 (ms)_ | 314.2 |
| _End-to-end TPS_ | 249.62 tx/s |
| _Backlog drain time (s)_ | 2.4 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.25 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 205.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
