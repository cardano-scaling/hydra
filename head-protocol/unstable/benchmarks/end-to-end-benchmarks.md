--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-16 18:03:45.034866451 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 448.0 |
| _P99_ | 450.1ms |
| _P95_ | 449.9ms |
| _P50_ | 448.0ms |
| _Tx validation time p50 (ms)_ | 394.5 |
| _End-to-end TPS_ | 666.22 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.44 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 767.9 |
| _P99_ | 881.4ms |
| _P95_ | 881.2ms |
| _P50_ | 795.7ms |
| _Tx validation time p50 (ms)_ | 344.8 |
| _End-to-end TPS_ | 1000.09 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.33 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 561.4 |
| _P99_ | 624.7ms |
| _P95_ | 624.5ms |
| _P50_ | 618.6ms |
| _Tx validation time p50 (ms)_ | 224.4 |
| _End-to-end TPS_ | 953.16 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.77 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 149.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
