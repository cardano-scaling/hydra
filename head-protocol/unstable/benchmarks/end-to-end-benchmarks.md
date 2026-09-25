--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-25 13:34:54.996504709 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 182.2 |
| _P99_ | 185.2ms |
| _P95_ | 185.0ms |
| _P50_ | 182.7ms |
| _Tx validation time p50 (ms)_ | 91.0 |
| _End-to-end TPS_ | 1615.52 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.77 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 921.2 |
| _P99_ | 987.0ms |
| _P95_ | 986.6ms |
| _P50_ | 968.8ms |
| _Tx validation time p50 (ms)_ | 367.7 |
| _End-to-end TPS_ | 901.26 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.00 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 797.9 |
| _P99_ | 904.8ms |
| _P95_ | 904.5ms |
| _P50_ | 750.6ms |
| _Tx validation time p50 (ms)_ | 198.2 |
| _End-to-end TPS_ | 651.67 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.26 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 149.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
