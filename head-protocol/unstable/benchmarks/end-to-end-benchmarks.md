--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-01 08:23:26.896646231 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 133.5 |
| _P99_ | 135.4ms |
| _P95_ | 135.2ms |
| _P50_ | 133.8ms |
| _Tx validation time p50 (ms)_ | 55.7 |
| _End-to-end TPS_ | 2209.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.73 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 806.8 |
| _P99_ | 818.8ms |
| _P95_ | 818.1ms |
| _P50_ | 809.2ms |
| _Tx validation time p50 (ms)_ | 300.0 |
| _End-to-end TPS_ | 1098.41 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.44 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 691.4 |
| _P99_ | 696.8ms |
| _P95_ | 696.4ms |
| _P50_ | 691.7ms |
| _Tx validation time p50 (ms)_ | 200.1 |
| _End-to-end TPS_ | 860.51 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.87 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 151.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
