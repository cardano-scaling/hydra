--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-26 21:27:20.597045678 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 156.9 |
| _P99_ | 158.7ms |
| _P95_ | 158.6ms |
| _P50_ | 157.2ms |
| _Tx validation time p50 (ms)_ | 109.0 |
| _End-to-end TPS_ | 1847.90 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 18.48 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 690.8 |
| _P99_ | 719.8ms |
| _P95_ | 719.5ms |
| _P50_ | 688.1ms |
| _Tx validation time p50 (ms)_ | 386.3 |
| _End-to-end TPS_ | 1237.67 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.13 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 762.9 |
| _P99_ | 855.7ms |
| _P95_ | 855.4ms |
| _P50_ | 847.7ms |
| _Tx validation time p50 (ms)_ | 312.5 |
| _End-to-end TPS_ | 696.85 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.48 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 154.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
