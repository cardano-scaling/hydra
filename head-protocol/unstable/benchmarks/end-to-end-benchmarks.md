--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-05 15:06:30.432228934 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 176.2 |
| _P99_ | 180.3ms |
| _P95_ | 180.0ms |
| _P50_ | 177.2ms |
| _Tx validation time p50 (ms)_ | 83.1 |
| _End-to-end TPS_ | 1659.80 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.07 /s |
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
| _Avg. Confirmation Time (ms)_ | 977.1 |
| _P99_ | 1050.3ms |
| _P95_ | 1049.3ms |
| _P50_ | 1013.4ms |
| _Tx validation time p50 (ms)_ | 395.4 |
| _End-to-end TPS_ | 851.75 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.84 /s |
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
| _Avg. Confirmation Time (ms)_ | 817.5 |
| _P99_ | 911.4ms |
| _P95_ | 911.0ms |
| _P50_ | 793.1ms |
| _Tx validation time p50 (ms)_ | 216.5 |
| _End-to-end TPS_ | 648.48 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.24 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 150.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
