--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-07 09:57:19.479540371 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 116.7 |
| _P99_ | 118.9ms |
| _P95_ | 118.8ms |
| _P50_ | 116.7ms |
| _Tx validation time p50 (ms)_ | 75.7 |
| _End-to-end TPS_ | 2466.44 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 24.66 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 558.4 |
| _P99_ | 593.9ms |
| _P95_ | 593.6ms |
| _P50_ | 580.5ms |
| _Tx validation time p50 (ms)_ | 281.9 |
| _End-to-end TPS_ | 1501.93 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 5.01 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 510.7 |
| _P99_ | 573.6ms |
| _P95_ | 573.5ms |
| _P50_ | 568.4ms |
| _Tx validation time p50 (ms)_ | 183.7 |
| _End-to-end TPS_ | 1018.95 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 6.79 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 148.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
