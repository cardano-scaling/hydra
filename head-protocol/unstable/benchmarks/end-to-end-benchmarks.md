--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-24 16:28:41.622907282 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 192.9 |
| _P99_ | 196.0ms |
| _P95_ | 195.5ms |
| _P50_ | 193.2ms |
| _Tx validation time p50 (ms)_ | 115.8 |
| _End-to-end TPS_ | 1480.01 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 14.80 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 956.8 |
| _P99_ | 974.9ms |
| _P95_ | 974.0ms |
| _P50_ | 966.0ms |
| _Tx validation time p50 (ms)_ | 519.6 |
| _End-to-end TPS_ | 891.84 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.97 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1894.3 |
| _P99_ | 2363.1ms |
| _P95_ | 2362.5ms |
| _P50_ | 2358.9ms |
| _Tx validation time p50 (ms)_ | 329.7 |
| _End-to-end TPS_ | 252.87 tx/s |
| _Backlog drain time (s)_ | 2.4 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.26 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 203.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
