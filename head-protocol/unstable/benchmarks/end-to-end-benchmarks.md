--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-24 14:57:13.709901096 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 188.0 |
| _P99_ | 191.0ms |
| _P95_ | 190.5ms |
| _P50_ | 188.6ms |
| _Tx validation time p50 (ms)_ | 69.6 |
| _End-to-end TPS_ | 1568.50 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.46 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 900.8 |
| _P99_ | 931.4ms |
| _P95_ | 930.5ms |
| _P50_ | 911.9ms |
| _Tx validation time p50 (ms)_ | 431.8 |
| _End-to-end TPS_ | 965.43 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.15 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 888.3 |
| _P99_ | 1003.3ms |
| _P95_ | 1002.3ms |
| _P50_ | 995.4ms |
| _Tx validation time p50 (ms)_ | 214.3 |
| _End-to-end TPS_ | 591.32 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.96 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 147.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
