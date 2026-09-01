--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-01 18:04:36.605641819 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 178.0 |
| _P99_ | 180.6ms |
| _P95_ | 180.4ms |
| _P50_ | 178.3ms |
| _Tx validation time p50 (ms)_ | 118.8 |
| _End-to-end TPS_ | 1625.91 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 16.26 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 914.3 |
| _P99_ | 955.8ms |
| _P95_ | 955.2ms |
| _P50_ | 931.6ms |
| _Tx validation time p50 (ms)_ | 472.2 |
| _End-to-end TPS_ | 933.59 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.11 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 867.5 |
| _P99_ | 1008.1ms |
| _P95_ | 1007.9ms |
| _P50_ | 962.9ms |
| _Tx validation time p50 (ms)_ | 312.0 |
| _End-to-end TPS_ | 588.79 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 4 |
| _Snapshots per second_ | 3.93 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 150.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
