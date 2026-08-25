--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-25 21:59:56.431321995 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 192.5 |
| _P99_ | 196.6ms |
| _P95_ | 196.5ms |
| _P50_ | 192.7ms |
| _Tx validation time p50 (ms)_ | 116.1 |
| _End-to-end TPS_ | 1475.70 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 14.76 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1596.8 |
| _P99_ | 1755.6ms |
| _P95_ | 1754.9ms |
| _P50_ | 1563.0ms |
| _Tx validation time p50 (ms)_ | 1045.6 |
| _End-to-end TPS_ | 508.61 tx/s |
| _Backlog drain time (s)_ | 1.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.70 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 980.2 |
| _P99_ | 1093.6ms |
| _P95_ | 1093.3ms |
| _P50_ | 887.2ms |
| _Tx validation time p50 (ms)_ | 398.2 |
| _End-to-end TPS_ | 545.13 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.73 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
