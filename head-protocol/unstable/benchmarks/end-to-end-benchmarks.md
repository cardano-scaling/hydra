--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-03 10:16:59.315992233 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 215.5 |
| _P99_ | 219.2ms |
| _P95_ | 219.1ms |
| _P50_ | 215.6ms |
| _Tx validation time p50 (ms)_ | 130.4 |
| _End-to-end TPS_ | 1342.24 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 13.42 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1071.3 |
| _P99_ | 1135.6ms |
| _P95_ | 1134.8ms |
| _P50_ | 1107.6ms |
| _Tx validation time p50 (ms)_ | 460.3 |
| _End-to-end TPS_ | 787.99 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.63 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 957.7 |
| _P99_ | 1073.2ms |
| _P95_ | 1072.8ms |
| _P50_ | 1069.3ms |
| _Tx validation time p50 (ms)_ | 336.0 |
| _End-to-end TPS_ | 553.38 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.77 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
