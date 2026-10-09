--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-09 07:56:17.506684512 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 194.7 |
| _P99_ | 198.4ms |
| _P95_ | 198.1ms |
| _P50_ | 194.9ms |
| _Tx validation time p50 (ms)_ | 106.5 |
| _End-to-end TPS_ | 1508.94 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.06 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 130.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 979.7 |
| _P99_ | 1044.9ms |
| _P95_ | 1044.3ms |
| _P50_ | 1020.9ms |
| _Tx validation time p50 (ms)_ | 380.3 |
| _End-to-end TPS_ | 854.92 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.85 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 147.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 763.6 |
| _P99_ | 819.4ms |
| _P95_ | 819.2ms |
| _P50_ | 757.9ms |
| _Tx validation time p50 (ms)_ | 214.0 |
| _End-to-end TPS_ | 716.58 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.58 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 148.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
