--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-20 13:43:52.132484444 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.3 |
| _P99_ | 149.0ms |
| _P95_ | 148.8ms |
| _P50_ | 147.7ms |
| _Tx validation time p50 (ms)_ | 71.1 |
| _End-to-end TPS_ | 2009.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.40 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 173.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 809.7 |
| _P99_ | 820.7ms |
| _P95_ | 820.0ms |
| _P50_ | 814.5ms |
| _Tx validation time p50 (ms)_ | 371.9 |
| _End-to-end TPS_ | 1092.96 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.43 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 173.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 800.5 |
| _P99_ | 931.7ms |
| _P95_ | 931.6ms |
| _P50_ | 751.3ms |
| _Tx validation time p50 (ms)_ | 240.3 |
| _End-to-end TPS_ | 638.92 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.19 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 184.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
