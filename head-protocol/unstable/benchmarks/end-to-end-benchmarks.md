--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-08-24 14:15:48.732269773 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 174.1 |
| _P99_ | 179.9ms |
| _P95_ | 179.7ms |
| _P50_ | 171.5ms |
| _Tx validation time p50 (ms)_ | 108.0 |
| _End-to-end TPS_ | 1640.50 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 16.40 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 939.0 |
| _P99_ | 948.4ms |
| _P95_ | 947.6ms |
| _P50_ | 939.4ms |
| _Tx validation time p50 (ms)_ | 500.1 |
| _End-to-end TPS_ | 936.85 tx/s |
| _Backlog drain time (s)_ | 0.9 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 3.12 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1871.5 |
| _P99_ | 2308.4ms |
| _P95_ | 2308.2ms |
| _P50_ | 2301.1ms |
| _Tx validation time p50 (ms)_ | 290.1 |
| _End-to-end TPS_ | 259.24 tx/s |
| _Backlog drain time (s)_ | 2.3 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 1.30 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 208.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
