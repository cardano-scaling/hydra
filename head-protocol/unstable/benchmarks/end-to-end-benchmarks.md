--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-18 13:56:09.288195754 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1663.8 |
| _P99_ | 1732.4ms |
| _P95_ | 1664.2ms |
| _P50_ | 1662.9ms |
| _Tx validation time p50 (ms)_ | 747.6 |
| _End-to-end TPS_ | 172.78 tx/s |
| _Backlog drain time (s)_ | 1.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.73 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1075.1 |
| _P99_ | 1081.6ms |
| _P95_ | 1081.3ms |
| _P50_ | 1076.1ms |
| _Tx validation time p50 (ms)_ | 613.5 |
| _End-to-end TPS_ | 829.12 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.84 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1169.5 |
| _P99_ | 1290.8ms |
| _P95_ | 1290.6ms |
| _P50_ | 1240.6ms |
| _Tx validation time p50 (ms)_ | 585.7 |
| _End-to-end TPS_ | 461.18 tx/s |
| _Backlog drain time (s)_ | 1.3 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.07 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 149.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
