--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-26 18:05:50.109071966 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 178.9 |
| _P99_ | 183.7ms |
| _P95_ | 183.5ms |
| _P50_ | 178.6ms |
| _Tx validation time p50 (ms)_ | 111.0 |
| _End-to-end TPS_ | 1589.04 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 15.89 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 869.6 |
| _P99_ | 907.1ms |
| _P95_ | 906.4ms |
| _P50_ | 892.7ms |
| _Tx validation time p50 (ms)_ | 432.5 |
| _End-to-end TPS_ | 978.83 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.26 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 852.2 |
| _P99_ | 1033.2ms |
| _P95_ | 1032.9ms |
| _P50_ | 948.2ms |
| _Tx validation time p50 (ms)_ | 308.0 |
| _End-to-end TPS_ | 572.83 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.82 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 149.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
