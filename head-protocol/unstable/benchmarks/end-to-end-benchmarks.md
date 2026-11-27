--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-27 12:13:45.524503025 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 322.0 |
| _P99_ | 323.8ms |
| _P95_ | 323.6ms |
| _P50_ | 322.2ms |
| _Tx validation time p50 (ms)_ | 215.8 |
| _End-to-end TPS_ | 925.57 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 6.17 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 761.5 |
| _P99_ | 807.2ms |
| _P95_ | 806.6ms |
| _P50_ | 747.0ms |
| _Tx validation time p50 (ms)_ | 311.6 |
| _End-to-end TPS_ | 1097.80 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.66 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 173.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1201.7 |
| _P99_ | 1208.3ms |
| _P95_ | 1207.7ms |
| _P50_ | 1202.4ms |
| _Tx validation time p50 (ms)_ | 472.9 |
| _End-to-end TPS_ | 496.38 tx/s |
| _Backlog drain time (s)_ | 1.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.65 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 185.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
