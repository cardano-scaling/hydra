--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-09-02 15:34:37.313132436 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 247.3 |
| _P99_ | 260.4ms |
| _P95_ | 260.2ms |
| _P50_ | 240.4ms |
| _Tx validation time p50 (ms)_ | 144.8 |
| _End-to-end TPS_ | 1124.75 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 11.25 /s |
| _Avg txs per snapshot_ | 100.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1004.9 |
| _P99_ | 1091.3ms |
| _P95_ | 1088.8ms |
| _P50_ | 1035.6ms |
| _Tx validation time p50 (ms)_ | 451.2 |
| _End-to-end TPS_ | 810.87 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.70 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 982.4 |
| _P99_ | 1103.0ms |
| _P95_ | 1102.7ms |
| _P50_ | 918.1ms |
| _Tx validation time p50 (ms)_ | 361.4 |
| _End-to-end TPS_ | 539.74 tx/s |
| _Backlog drain time (s)_ | 1.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.70 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 148.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 1000 |
      
