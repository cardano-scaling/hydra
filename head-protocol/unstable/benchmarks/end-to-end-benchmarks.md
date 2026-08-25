--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-25 12:18:58.089667528 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 155.0 |
| _P99_ | 156.6ms |
| _P95_ | 156.5ms |
| _P50_ | 155.1ms |
| _Tx validation time p50 (ms)_ | 123.9 |
| _End-to-end TPS_ | 1898.08 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 18.98 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 943.0 |
| _P99_ | 997.3ms |
| _P95_ | 996.2ms |
| _P50_ | 974.2ms |
| _Tx validation time p50 (ms)_ | 614.1 |
| _End-to-end TPS_ | 896.95 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.99 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 713.1 |
| _P99_ | 798.7ms |
| _P95_ | 798.6ms |
| _P50_ | 659.9ms |
| _Tx validation time p50 (ms)_ | 371.1 |
| _End-to-end TPS_ | 748.36 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.74 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
