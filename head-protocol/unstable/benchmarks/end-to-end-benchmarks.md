--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-02 15:03:33.006483494 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 108.4 |
| _P99_ | 110.2ms |
| _P95_ | 110.0ms |
| _P50_ | 108.6ms |
| _Tx validation time p50 (ms)_ | 36.9 |
| _End-to-end TPS_ | 2715.20 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.10 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 509.7 |
| _P99_ | 535.8ms |
| _P95_ | 535.2ms |
| _P50_ | 526.4ms |
| _Tx validation time p50 (ms)_ | 247.0 |
| _End-to-end TPS_ | 1628.83 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 5.43 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 173.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 426.6 |
| _P99_ | 431.3ms |
| _P95_ | 430.8ms |
| _P50_ | 427.2ms |
| _Tx validation time p50 (ms)_ | 128.2 |
| _End-to-end TPS_ | 1389.96 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.63 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 181.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
