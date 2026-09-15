--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-15 21:49:04.066927431 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 399.9 |
| _P99_ | 401.8ms |
| _P95_ | 401.7ms |
| _P50_ | 400.4ms |
| _Tx validation time p50 (ms)_ | 287.0 |
| _End-to-end TPS_ | 745.66 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.97 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 130.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1329.5 |
| _P99_ | 1366.7ms |
| _P95_ | 1366.5ms |
| _P50_ | 1314.5ms |
| _Tx validation time p50 (ms)_ | 779.4 |
| _End-to-end TPS_ | 655.04 tx/s |
| _Backlog drain time (s)_ | 1.4 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.18 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 991.7 |
| _P99_ | 1178.1ms |
| _P95_ | 1177.9ms |
| _P50_ | 1173.4ms |
| _Tx validation time p50 (ms)_ | 248.6 |
| _End-to-end TPS_ | 507.08 tx/s |
| _Backlog drain time (s)_ | 1.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.54 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
