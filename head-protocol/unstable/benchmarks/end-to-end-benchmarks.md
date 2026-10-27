--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-27 08:54:12.81841511 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 198.4 |
| _P99_ | 201.5ms |
| _P95_ | 201.3ms |
| _P50_ | 198.7ms |
| _Tx validation time p50 (ms)_ | 97.0 |
| _End-to-end TPS_ | 1485.51 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.90 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 133.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 963.4 |
| _P99_ | 1041.0ms |
| _P95_ | 1040.4ms |
| _P50_ | 941.6ms |
| _Tx validation time p50 (ms)_ | 388.0 |
| _End-to-end TPS_ | 860.15 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.87 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 810.7 |
| _P99_ | 913.0ms |
| _P95_ | 912.4ms |
| _P50_ | 766.2ms |
| _Tx validation time p50 (ms)_ | 222.4 |
| _End-to-end TPS_ | 644.09 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.22 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 146.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
