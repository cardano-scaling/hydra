--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-25 14:28:44.34565955 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 196.0 |
| _P99_ | 200.3ms |
| _P95_ | 199.9ms |
| _P50_ | 197.0ms |
| _Tx validation time p50 (ms)_ | 100.1 |
| _End-to-end TPS_ | 1495.08 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.97 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 867.0 |
| _P99_ | 893.5ms |
| _P95_ | 886.8ms |
| _P50_ | 867.2ms |
| _Tx validation time p50 (ms)_ | 368.7 |
| _End-to-end TPS_ | 1000.09 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.33 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 782.9 |
| _P99_ | 790.5ms |
| _P95_ | 790.0ms |
| _P50_ | 784.6ms |
| _Tx validation time p50 (ms)_ | 218.0 |
| _End-to-end TPS_ | 758.45 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.53 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 156.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
