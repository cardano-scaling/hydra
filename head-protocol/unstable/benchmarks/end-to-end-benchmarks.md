--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-24 08:28:31.391720955 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Avg. Confirmation Time (ms)_ | 185.2 |
| _P99_ | 187.6ms |
| _P95_ | 187.5ms |
| _P50_ | 185.4ms |
| _Tx validation time p50 (ms)_ | 129.7 |
| _End-to-end TPS_ | 1579.95 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 15.80 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Avg. Confirmation Time (ms)_ | 929.1 |
| _P99_ | 972.1ms |
| _P95_ | 971.3ms |
| _P50_ | 914.9ms |
| _Tx validation time p50 (ms)_ | 461.3 |
| _End-to-end TPS_ | 919.33 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.06 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Avg. Confirmation Time (ms)_ | 1796.7 |
| _P99_ | 2230.9ms |
| _P95_ | 2230.5ms |
| _P50_ | 2220.3ms |
| _Tx validation time p50 (ms)_ | 298.7 |
| _End-to-end TPS_ | 268.03 tx/s |
| _Backlog drain time (s)_ | 2.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.34 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 203.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
