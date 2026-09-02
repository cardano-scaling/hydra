--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-02 17:55:39.505133407 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 199.1 |
| _P99_ | 207.6ms |
| _P95_ | 207.6ms |
| _P50_ | 197.2ms |
| _Tx validation time p50 (ms)_ | 117.8 |
| _End-to-end TPS_ | 1428.67 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 14.29 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 944.6 |
| _P99_ | 963.3ms |
| _P95_ | 962.5ms |
| _P50_ | 947.0ms |
| _Tx validation time p50 (ms)_ | 493.6 |
| _End-to-end TPS_ | 933.88 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.08 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 888.9 |
| _P99_ | 991.6ms |
| _P95_ | 991.4ms |
| _P50_ | 984.0ms |
| _Tx validation time p50 (ms)_ | 326.8 |
| _End-to-end TPS_ | 602.22 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.01 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 154.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
