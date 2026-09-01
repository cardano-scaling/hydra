--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-01 16:37:34.412991404 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 189.9 |
| _P99_ | 193.2ms |
| _P95_ | 193.1ms |
| _P50_ | 190.1ms |
| _Tx validation time p50 (ms)_ | 120.5 |
| _End-to-end TPS_ | 1509.18 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 15.09 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 130.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 874.8 |
| _P99_ | 892.8ms |
| _P95_ | 891.9ms |
| _P50_ | 871.0ms |
| _Tx validation time p50 (ms)_ | 493.2 |
| _End-to-end TPS_ | 985.14 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.28 /s |
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
| _Avg. Confirmation Time (ms)_ | 844.4 |
| _P99_ | 947.2ms |
| _P95_ | 946.5ms |
| _P50_ | 939.0ms |
| _Tx validation time p50 (ms)_ | 280.5 |
| _End-to-end TPS_ | 627.84 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.14 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 150.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
