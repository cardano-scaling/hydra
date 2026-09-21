--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-21 09:40:23.901214037 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 225.8 |
| _P99_ | 236.1ms |
| _P95_ | 235.3ms |
| _P50_ | 222.8ms |
| _Tx validation time p50 (ms)_ | 126.9 |
| _End-to-end TPS_ | 1248.53 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 12.49 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 130.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1124.6 |
| _P99_ | 1194.3ms |
| _P95_ | 1193.6ms |
| _P50_ | 1173.1ms |
| _Tx validation time p50 (ms)_ | 565.5 |
| _End-to-end TPS_ | 750.94 tx/s |
| _Backlog drain time (s)_ | 1.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.50 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 924.6 |
| _P99_ | 1028.2ms |
| _P95_ | 1027.7ms |
| _P50_ | 1023.1ms |
| _Tx validation time p50 (ms)_ | 282.8 |
| _End-to-end TPS_ | 573.26 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.82 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 146.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
