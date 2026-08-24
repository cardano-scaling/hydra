--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-24 14:26:49.321087407 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 189.3 |
| _P99_ | 193.6ms |
| _P95_ | 193.5ms |
| _P50_ | 189.5ms |
| _Tx validation time p50 (ms)_ | 119.5 |
| _End-to-end TPS_ | 1491.46 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 14.91 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 974.7 |
| _P99_ | 1033.1ms |
| _P95_ | 1032.5ms |
| _P50_ | 962.8ms |
| _Tx validation time p50 (ms)_ | 509.2 |
| _End-to-end TPS_ | 862.12 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.87 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1813.3 |
| _P99_ | 2421.0ms |
| _P95_ | 2420.6ms |
| _P50_ | 2191.9ms |
| _Tx validation time p50 (ms)_ | 310.7 |
| _End-to-end TPS_ | 246.61 tx/s |
| _Backlog drain time (s)_ | 2.4 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 1.64 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 197.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
