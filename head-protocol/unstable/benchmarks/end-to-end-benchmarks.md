--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-29 11:02:01.517953662 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 165.4 |
| _P99_ | 168.4ms |
| _P95_ | 168.2ms |
| _P50_ | 165.9ms |
| _Tx validation time p50 (ms)_ | 75.6 |
| _End-to-end TPS_ | 1777.14 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.85 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 131.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 874.4 |
| _P99_ | 921.1ms |
| _P95_ | 920.4ms |
| _P50_ | 862.4ms |
| _Tx validation time p50 (ms)_ | 375.7 |
| _End-to-end TPS_ | 957.43 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.19 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 147.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 867.0 |
| _P99_ | 991.2ms |
| _P95_ | 991.0ms |
| _P50_ | 762.6ms |
| _Tx validation time p50 (ms)_ | 232.4 |
| _End-to-end TPS_ | 602.21 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.01 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 153.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
