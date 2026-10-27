--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-27 14:48:35.106577601 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 194.5 |
| _P99_ | 196.9ms |
| _P95_ | 196.6ms |
| _P50_ | 194.9ms |
| _Tx validation time p50 (ms)_ | 103.8 |
| _End-to-end TPS_ | 1520.68 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.14 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 136.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 928.1 |
| _P99_ | 994.0ms |
| _P95_ | 993.5ms |
| _P50_ | 894.1ms |
| _Tx validation time p50 (ms)_ | 388.3 |
| _End-to-end TPS_ | 884.59 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.95 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 752.7 |
| _P99_ | 828.4ms |
| _P95_ | 828.2ms |
| _P50_ | 735.2ms |
| _Tx validation time p50 (ms)_ | 222.2 |
| _End-to-end TPS_ | 711.72 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.56 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 153.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
