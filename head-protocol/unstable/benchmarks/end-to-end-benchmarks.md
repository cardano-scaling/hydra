--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-02 06:37:38.205532052 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 103.7 |
| _P99_ | 105.8ms |
| _P95_ | 105.6ms |
| _P50_ | 104.1ms |
| _Tx validation time p50 (ms)_ | 53.6 |
| _End-to-end TPS_ | 2829.78 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.87 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 136.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 607.6 |
| _P99_ | 623.9ms |
| _P95_ | 621.2ms |
| _P50_ | 607.2ms |
| _Tx validation time p50 (ms)_ | 313.7 |
| _End-to-end TPS_ | 1397.21 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.66 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 462.4 |
| _P99_ | 475.9ms |
| _P95_ | 466.8ms |
| _P50_ | 462.2ms |
| _Tx validation time p50 (ms)_ | 169.4 |
| _End-to-end TPS_ | 1233.28 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 6.17 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
