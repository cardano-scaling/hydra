--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-06 13:34:34.129933052 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 159.7 |
| _P99_ | 161.9ms |
| _P95_ | 161.6ms |
| _P50_ | 160.0ms |
| _Tx validation time p50 (ms)_ | 69.9 |
| _End-to-end TPS_ | 1849.03 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.33 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 845.1 |
| _P99_ | 863.3ms |
| _P95_ | 862.9ms |
| _P50_ | 847.5ms |
| _Tx validation time p50 (ms)_ | 364.1 |
| _End-to-end TPS_ | 1013.25 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.38 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 758.9 |
| _P99_ | 766.2ms |
| _P95_ | 765.8ms |
| _P50_ | 759.2ms |
| _Tx validation time p50 (ms)_ | 194.0 |
| _End-to-end TPS_ | 782.45 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.61 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 157.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
