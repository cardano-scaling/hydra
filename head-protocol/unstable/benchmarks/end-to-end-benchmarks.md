--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-05 13:32:24.804288867 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 190.0 |
| _P99_ | 192.2ms |
| _P95_ | 191.8ms |
| _P50_ | 190.2ms |
| _Tx validation time p50 (ms)_ | 94.8 |
| _End-to-end TPS_ | 1557.17 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.38 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 923.7 |
| _P99_ | 1003.2ms |
| _P95_ | 1002.6ms |
| _P50_ | 919.1ms |
| _Tx validation time p50 (ms)_ | 412.1 |
| _End-to-end TPS_ | 889.57 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.97 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 792.5 |
| _P99_ | 840.8ms |
| _P95_ | 840.5ms |
| _P50_ | 789.1ms |
| _Tx validation time p50 (ms)_ | 216.9 |
| _End-to-end TPS_ | 704.49 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.52 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 146.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
