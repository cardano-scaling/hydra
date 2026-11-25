--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-25 18:18:59.918660514 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 118.6 |
| _P99_ | 120.7ms |
| _P95_ | 120.5ms |
| _P50_ | 118.9ms |
| _Tx validation time p50 (ms)_ | 57.1 |
| _End-to-end TPS_ | 2482.42 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.55 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 171.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 577.2 |
| _P99_ | 581.9ms |
| _P95_ | 581.4ms |
| _P50_ | 577.9ms |
| _Tx validation time p50 (ms)_ | 251.2 |
| _End-to-end TPS_ | 1502.10 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 5.01 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 172.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 456.4 |
| _P99_ | 463.1ms |
| _P95_ | 462.7ms |
| _P50_ | 457.9ms |
| _Tx validation time p50 (ms)_ | 131.9 |
| _End-to-end TPS_ | 1294.43 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.31 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 186.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
