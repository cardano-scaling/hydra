--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-15 08:36:09.414882667 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 220.6 |
| _P99_ | 228.0ms |
| _P95_ | 227.8ms |
| _P50_ | 220.0ms |
| _Tx validation time p50 (ms)_ | 122.8 |
| _End-to-end TPS_ | 1278.83 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 12.79 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1072.3 |
| _P99_ | 1148.8ms |
| _P95_ | 1148.0ms |
| _P50_ | 1108.0ms |
| _Tx validation time p50 (ms)_ | 474.3 |
| _End-to-end TPS_ | 776.93 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.59 /s |
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
| _Avg. Confirmation Time (ms)_ | 960.1 |
| _P99_ | 1092.9ms |
| _P95_ | 1072.7ms |
| _P50_ | 1057.5ms |
| _Tx validation time p50 (ms)_ | 303.0 |
| _End-to-end TPS_ | 539.20 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.59 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 147.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
