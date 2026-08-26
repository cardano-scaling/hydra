--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-26 10:35:47.747435607 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 183.9 |
| _P99_ | 186.0ms |
| _P95_ | 185.6ms |
| _P50_ | 184.3ms |
| _Tx validation time p50 (ms)_ | 124.0 |
| _End-to-end TPS_ | 1582.25 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 15.82 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 923.7 |
| _P99_ | 932.1ms |
| _P95_ | 931.3ms |
| _P50_ | 927.6ms |
| _Tx validation time p50 (ms)_ | 538.1 |
| _End-to-end TPS_ | 954.55 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.18 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 857.9 |
| _P99_ | 1017.9ms |
| _P95_ | 1017.7ms |
| _P50_ | 951.4ms |
| _Tx validation time p50 (ms)_ | 312.0 |
| _End-to-end TPS_ | 584.73 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.90 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 151.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
