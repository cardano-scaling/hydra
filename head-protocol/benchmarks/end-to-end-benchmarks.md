--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-26 11:26:14.605606328 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 203.8 |
| _P99_ | 207.7ms |
| _P95_ | 207.4ms |
| _P50_ | 203.3ms |
| _Tx validation time p50 (ms)_ | 98.7 |
| _End-to-end TPS_ | 1441.59 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.61 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 132.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 999.0 |
| _P99_ | 1014.6ms |
| _P95_ | 1013.4ms |
| _P50_ | 999.5ms |
| _Tx validation time p50 (ms)_ | 455.2 |
| _End-to-end TPS_ | 883.71 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.96 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 757.9 |
| _P99_ | 778.2ms |
| _P95_ | 771.8ms |
| _P50_ | 761.3ms |
| _Tx validation time p50 (ms)_ | 218.2 |
| _End-to-end TPS_ | 744.19 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.72 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 150.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
