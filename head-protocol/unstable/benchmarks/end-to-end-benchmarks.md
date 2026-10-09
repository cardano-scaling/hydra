--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-09 12:57:58.793957197 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 152.9 |
| _P99_ | 154.9ms |
| _P95_ | 154.7ms |
| _P50_ | 153.2ms |
| _Tx validation time p50 (ms)_ | 47.0 |
| _End-to-end TPS_ | 1933.07 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.89 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 594.1 |
| _P99_ | 633.6ms |
| _P95_ | 633.2ms |
| _P50_ | 579.4ms |
| _Tx validation time p50 (ms)_ | 269.1 |
| _End-to-end TPS_ | 1404.28 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.68 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 433.5 |
| _P99_ | 441.0ms |
| _P95_ | 439.9ms |
| _P50_ | 434.9ms |
| _Tx validation time p50 (ms)_ | 137.8 |
| _End-to-end TPS_ | 1359.38 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.53 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 158.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
