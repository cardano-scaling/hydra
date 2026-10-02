--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-02 13:31:56.034993301 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.9 |
| _P99_ | 143.1ms |
| _P95_ | 142.9ms |
| _P50_ | 141.2ms |
| _Tx validation time p50 (ms)_ | 62.1 |
| _End-to-end TPS_ | 2091.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.94 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 685.8 |
| _P99_ | 727.6ms |
| _P95_ | 726.4ms |
| _P50_ | 715.3ms |
| _Tx validation time p50 (ms)_ | 243.9 |
| _End-to-end TPS_ | 1224.22 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.08 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 644.9 |
| _P99_ | 652.0ms |
| _P95_ | 651.1ms |
| _P50_ | 645.5ms |
| _Tx validation time p50 (ms)_ | 157.1 |
| _End-to-end TPS_ | 919.47 tx/s |
| _Backlog drain time (s)_ | 0.6 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.06 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 152.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
