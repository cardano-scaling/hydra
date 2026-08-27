--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-27 13:55:27.708062374 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 193.5 |
| _P99_ | 196.4ms |
| _P95_ | 196.2ms |
| _P50_ | 193.4ms |
| _Tx validation time p50 (ms)_ | 128.0 |
| _End-to-end TPS_ | 1494.46 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 14.94 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 870.8 |
| _P99_ | 885.3ms |
| _P95_ | 884.8ms |
| _P50_ | 872.7ms |
| _Tx validation time p50 (ms)_ | 487.2 |
| _End-to-end TPS_ | 1011.47 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.25 /s |
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
| _Avg. Confirmation Time (ms)_ | 888.8 |
| _P99_ | 990.5ms |
| _P95_ | 990.3ms |
| _P50_ | 984.4ms |
| _Tx validation time p50 (ms)_ | 342.7 |
| _End-to-end TPS_ | 602.61 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.01 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 150.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
