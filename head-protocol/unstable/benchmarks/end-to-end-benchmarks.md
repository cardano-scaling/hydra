--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-03 09:29:45.717170682 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 177.4 |
| _P99_ | 184.3ms |
| _P95_ | 183.8ms |
| _P50_ | 180.5ms |
| _Tx validation time p50 (ms)_ | 79.3 |
| _End-to-end TPS_ | 1623.51 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.82 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1032.3 |
| _P99_ | 1071.1ms |
| _P95_ | 1069.2ms |
| _P50_ | 1061.1ms |
| _Tx validation time p50 (ms)_ | 387.7 |
| _End-to-end TPS_ | 831.15 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.77 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 172.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 819.6 |
| _P99_ | 925.8ms |
| _P95_ | 924.9ms |
| _P50_ | 760.9ms |
| _Tx validation time p50 (ms)_ | 203.9 |
| _End-to-end TPS_ | 634.78 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.17 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 182.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
