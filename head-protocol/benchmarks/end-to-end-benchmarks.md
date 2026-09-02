--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-02 17:31:50.200717992 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 217.8 |
| _P99_ | 221.8ms |
| _P95_ | 221.4ms |
| _P50_ | 217.8ms |
| _Tx validation time p50 (ms)_ | 127.8 |
| _End-to-end TPS_ | 1350.09 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.00 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 130.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1017.6 |
| _P99_ | 1082.2ms |
| _P95_ | 1081.6ms |
| _P50_ | 1063.7ms |
| _Tx validation time p50 (ms)_ | 453.3 |
| _End-to-end TPS_ | 827.95 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.76 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 922.4 |
| _P99_ | 1040.9ms |
| _P95_ | 1040.7ms |
| _P50_ | 1032.1ms |
| _Tx validation time p50 (ms)_ | 313.9 |
| _End-to-end TPS_ | 572.97 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.86 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
