--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-24 11:16:31.2265917 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 198.7 |
| _P99_ | 201.6ms |
| _P95_ | 201.3ms |
| _P50_ | 199.2ms |
| _Tx validation time p50 (ms)_ | 128.1 |
| _End-to-end TPS_ | 1486.28 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.91 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 918.1 |
| _P99_ | 972.4ms |
| _P95_ | 959.6ms |
| _P50_ | 906.7ms |
| _Tx validation time p50 (ms)_ | 461.9 |
| _End-to-end TPS_ | 919.68 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.07 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Avg. Confirmation Time (ms)_ | 1854.4 |
| _P99_ | 2378.6ms |
| _P95_ | 2317.0ms |
| _P50_ | 2308.4ms |
| _Tx validation time p50 (ms)_ | 328.7 |
| _End-to-end TPS_ | 250.65 tx/s |
| _Backlog drain time (s)_ | 2.4 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 1.67 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 200.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
