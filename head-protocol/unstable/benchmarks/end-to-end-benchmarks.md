--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-06 13:47:24.744407746 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.7 |
| _P99_ | 145.8ms |
| _P95_ | 145.7ms |
| _P50_ | 144.9ms |
| _Tx validation time p50 (ms)_ | 55.1 |
| _End-to-end TPS_ | 2054.01 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.69 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 492.7 |
| _P99_ | 511.5ms |
| _P95_ | 505.8ms |
| _P50_ | 491.9ms |
| _Tx validation time p50 (ms)_ | 201.7 |
| _End-to-end TPS_ | 1746.46 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 5.82 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 556.5 |
| _P99_ | 560.6ms |
| _P95_ | 560.4ms |
| _P50_ | 557.2ms |
| _Tx validation time p50 (ms)_ | 151.2 |
| _End-to-end TPS_ | 1069.58 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.57 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 156.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
