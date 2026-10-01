--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-01 08:24:10.795422327 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 175.3 |
| _P99_ | 176.9ms |
| _P95_ | 176.8ms |
| _P50_ | 175.5ms |
| _Tx validation time p50 (ms)_ | 92.3 |
| _End-to-end TPS_ | 1691.83 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.28 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 705.9 |
| _P99_ | 715.6ms |
| _P95_ | 715.3ms |
| _P50_ | 711.9ms |
| _Tx validation time p50 (ms)_ | 336.8 |
| _End-to-end TPS_ | 1246.11 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.77 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 600.6 |
| _P99_ | 681.3ms |
| _P95_ | 681.1ms |
| _P50_ | 564.0ms |
| _Tx validation time p50 (ms)_ | 143.1 |
| _End-to-end TPS_ | 867.99 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 4.34 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 150.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
