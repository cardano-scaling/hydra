--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-01 10:36:41.009106471 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 212.1 |
| _P99_ | 217.2ms |
| _P95_ | 216.8ms |
| _P50_ | 212.0ms |
| _Tx validation time p50 (ms)_ | 135.0 |
| _End-to-end TPS_ | 1340.69 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 13.41 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 130.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 939.3 |
| _P99_ | 992.4ms |
| _P95_ | 988.0ms |
| _P50_ | 961.6ms |
| _Tx validation time p50 (ms)_ | 467.3 |
| _End-to-end TPS_ | 895.00 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.98 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 863.3 |
| _P99_ | 1047.3ms |
| _P95_ | 1040.9ms |
| _P50_ | 944.5ms |
| _Tx validation time p50 (ms)_ | 282.2 |
| _End-to-end TPS_ | 565.30 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.77 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 150.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
