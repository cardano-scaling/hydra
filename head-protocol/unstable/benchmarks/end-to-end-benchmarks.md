--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-22 18:19:07.321789654 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 191.6 |
| _P99_ | 195.8ms |
| _P95_ | 195.3ms |
| _P50_ | 191.9ms |
| _Tx validation time p50 (ms)_ | 89.3 |
| _End-to-end TPS_ | 1528.94 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.19 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 904.6 |
| _P99_ | 963.5ms |
| _P95_ | 954.6ms |
| _P50_ | 900.2ms |
| _Tx validation time p50 (ms)_ | 382.0 |
| _End-to-end TPS_ | 916.15 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.05 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 910.2 |
| _P99_ | 988.9ms |
| _P95_ | 988.1ms |
| _P50_ | 978.3ms |
| _Tx validation time p50 (ms)_ | 189.3 |
| _End-to-end TPS_ | 603.94 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.02 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 149.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
