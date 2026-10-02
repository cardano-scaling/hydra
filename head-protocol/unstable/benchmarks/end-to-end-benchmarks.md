--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-02 14:08:34.544087323 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 597.2 |
| _P99_ | 598.5ms |
| _P95_ | 598.4ms |
| _P50_ | 597.6ms |
| _Tx validation time p50 (ms)_ | 179.3 |
| _End-to-end TPS_ | 500.94 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.34 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 129.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1329.9 |
| _P99_ | 1362.7ms |
| _P95_ | 1362.0ms |
| _P50_ | 1327.3ms |
| _Tx validation time p50 (ms)_ | 521.8 |
| _End-to-end TPS_ | 647.08 tx/s |
| _Backlog drain time (s)_ | 1.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.44 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1087.9 |
| _P99_ | 1092.6ms |
| _P95_ | 1092.3ms |
| _P50_ | 1088.7ms |
| _Tx validation time p50 (ms)_ | 296.6 |
| _End-to-end TPS_ | 548.97 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.83 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 156.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
