--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-10-02 10:39:49.906645557 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 225.7 |
| _P99_ | 234.1ms |
| _P95_ | 234.0ms |
| _P50_ | 224.4ms |
| _Tx validation time p50 (ms)_ | 120.7 |
| _End-to-end TPS_ | 1242.75 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 12.43 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 131.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1058.7 |
| _P99_ | 1140.4ms |
| _P95_ | 1139.7ms |
| _P50_ | 1083.9ms |
| _Tx validation time p50 (ms)_ | 458.0 |
| _End-to-end TPS_ | 783.33 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.61 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 146.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 936.3 |
| _P99_ | 1055.4ms |
| _P95_ | 1055.0ms |
| _P50_ | 1051.3ms |
| _Tx validation time p50 (ms)_ | 305.7 |
| _End-to-end TPS_ | 560.26 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.80 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 151.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
