--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-18 10:07:12.093998992 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 328.5 |
| _P99_ | 363.3ms |
| _P95_ | 363.2ms |
| _P50_ | 320.9ms |
| _Tx validation time p50 (ms)_ | 251.4 |
| _End-to-end TPS_ | 818.29 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 8.18 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1171.7 |
| _P99_ | 1186.1ms |
| _P95_ | 1185.4ms |
| _P50_ | 1180.7ms |
| _Tx validation time p50 (ms)_ | 477.1 |
| _End-to-end TPS_ | 755.64 tx/s |
| _Backlog drain time (s)_ | 1.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.68 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1061.4 |
| _P99_ | 1155.5ms |
| _P95_ | 1155.2ms |
| _P50_ | 1151.7ms |
| _Tx validation time p50 (ms)_ | 305.1 |
| _End-to-end TPS_ | 515.40 tx/s |
| _Backlog drain time (s)_ | 1.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.58 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
