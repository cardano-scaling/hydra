--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-21 14:42:57.002726117 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 193.4 |
| _P99_ | 196.0ms |
| _P95_ | 195.7ms |
| _P50_ | 193.6ms |
| _Tx validation time p50 (ms)_ | 99.5 |
| _End-to-end TPS_ | 1526.40 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.18 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 991.1 |
| _P99_ | 1056.9ms |
| _P95_ | 1056.1ms |
| _P50_ | 1048.6ms |
| _Tx validation time p50 (ms)_ | 375.6 |
| _End-to-end TPS_ | 826.06 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.75 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 838.6 |
| _P99_ | 1052.9ms |
| _P95_ | 1052.6ms |
| _P50_ | 898.3ms |
| _Tx validation time p50 (ms)_ | 404.3 |
| _End-to-end TPS_ | 565.76 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 5 |
| _Snapshots per second_ | 4.71 /s |
| _Avg txs per snapshot_ | 120.0 |
| _Peak node RSS (MB)_ | 149.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
