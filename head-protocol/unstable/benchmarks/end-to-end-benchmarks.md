--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-24 13:47:50.886903761 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 103.4 |
| _P99_ | 105.6ms |
| _P95_ | 105.4ms |
| _P50_ | 103.8ms |
| _Tx validation time p50 (ms)_ | 30.1 |
| _End-to-end TPS_ | 2829.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.87 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 484.0 |
| _P99_ | 518.8ms |
| _P95_ | 518.5ms |
| _P50_ | 471.0ms |
| _Tx validation time p50 (ms)_ | 203.2 |
| _End-to-end TPS_ | 1709.63 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 5.70 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 386.3 |
| _P99_ | 408.4ms |
| _P95_ | 408.2ms |
| _P50_ | 384.6ms |
| _Tx validation time p50 (ms)_ | 100.5 |
| _End-to-end TPS_ | 1435.95 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 7.18 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 153.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
