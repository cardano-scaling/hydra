--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-02 10:29:37.362963365 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 610.3 |
| _P99_ | 612.0ms |
| _P95_ | 611.9ms |
| _P50_ | 610.6ms |
| _Tx validation time p50 (ms)_ | 111.4 |
| _End-to-end TPS_ | 489.91 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.27 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 132.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 684.1 |
| _P99_ | 706.5ms |
| _P95_ | 706.2ms |
| _P50_ | 680.3ms |
| _Tx validation time p50 (ms)_ | 260.3 |
| _End-to-end TPS_ | 1267.13 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.22 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 865.2 |
| _P99_ | 869.3ms |
| _P95_ | 868.9ms |
| _P50_ | 866.0ms |
| _Tx validation time p50 (ms)_ | 325.0 |
| _End-to-end TPS_ | 689.91 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.30 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 155.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
