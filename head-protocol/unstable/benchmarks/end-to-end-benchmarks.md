--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-26 12:26:03.802358667 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.3 |
| _P99_ | 155.9ms |
| _P95_ | 155.7ms |
| _P50_ | 154.5ms |
| _Tx validation time p50 (ms)_ | 107.0 |
| _End-to-end TPS_ | 1888.62 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 18.89 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 730.8 |
| _P99_ | 766.2ms |
| _P95_ | 765.8ms |
| _P50_ | 739.5ms |
| _Tx validation time p50 (ms)_ | 408.8 |
| _End-to-end TPS_ | 1171.01 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.90 /s |
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
| _Avg. Confirmation Time (ms)_ | 908.4 |
| _P99_ | 988.7ms |
| _P95_ | 988.6ms |
| _P50_ | 858.4ms |
| _Tx validation time p50 (ms)_ | 422.5 |
| _End-to-end TPS_ | 604.03 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.02 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 152.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
