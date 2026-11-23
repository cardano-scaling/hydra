--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-23 14:22:36.002871862 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 121.4 |
| _P99_ | 123.5ms |
| _P95_ | 123.3ms |
| _P50_ | 121.8ms |
| _Tx validation time p50 (ms)_ | 65.9 |
| _End-to-end TPS_ | 2423.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.16 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 171.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 531.5 |
| _P99_ | 551.0ms |
| _P95_ | 550.4ms |
| _P50_ | 543.5ms |
| _Tx validation time p50 (ms)_ | 227.3 |
| _End-to-end TPS_ | 1624.51 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.61 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 172.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 443.3 |
| _P99_ | 449.1ms |
| _P95_ | 448.5ms |
| _P50_ | 444.1ms |
| _Tx validation time p50 (ms)_ | 127.1 |
| _End-to-end TPS_ | 1335.10 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.45 /s |
| _Avg txs per snapshot_ | 300.0 |
| _Peak node RSS (MB)_ | 183.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
