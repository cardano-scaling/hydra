--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-06 12:18:16.234512657 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 639.6 |
| _P99_ | 642.3ms |
| _P95_ | 642.1ms |
| _P50_ | 640.1ms |
| _Tx validation time p50 (ms)_ | 448.3 |
| _End-to-end TPS_ | 466.84 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.11 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 132.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 847.3 |
| _P99_ | 864.1ms |
| _P95_ | 863.4ms |
| _P50_ | 849.0ms |
| _Tx validation time p50 (ms)_ | 319.4 |
| _End-to-end TPS_ | 1003.56 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.35 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 720.3 |
| _P99_ | 840.4ms |
| _P95_ | 834.7ms |
| _P50_ | 696.5ms |
| _Tx validation time p50 (ms)_ | 264.2 |
| _End-to-end TPS_ | 708.25 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.54 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 149.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
