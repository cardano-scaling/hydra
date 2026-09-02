--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-02 07:01:53.483630968 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 280.7 |
| _P99_ | 283.4ms |
| _P95_ | 283.3ms |
| _P50_ | 280.6ms |
| _Tx validation time p50 (ms)_ | 228.1 |
| _End-to-end TPS_ | 1044.06 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 10.44 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1374.8 |
| _P99_ | 1500.7ms |
| _P95_ | 1500.4ms |
| _P50_ | 1321.5ms |
| _Tx validation time p50 (ms)_ | 772.6 |
| _End-to-end TPS_ | 594.76 tx/s |
| _Backlog drain time (s)_ | 1.5 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.98 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1473.9 |
| _P99_ | 1672.0ms |
| _P95_ | 1671.8ms |
| _P50_ | 1347.6ms |
| _Tx validation time p50 (ms)_ | 679.5 |
| _End-to-end TPS_ | 357.21 tx/s |
| _Backlog drain time (s)_ | 1.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.79 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 153.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
