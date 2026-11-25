--- 
sidebar_label: 'End-to-end benchmarks' 
sidebar_position: 4 
--- 

# End-to-end benchmark results 

This page is intended to collect the latest end-to-end benchmark  results produced by Hydra's continuous integration (CI) system from  the latest `master` code.

:::caution

Please note that these results are approximate  as they are currently produced from limited cloud VMs and not controlled hardware.  Rather than focusing on the absolute results,   the emphasis should be on relative results,  such as how the timings for a scenario evolve as the code changes.

:::

_Generated at_  2026-11-25 13:12:16.841153682 UTC


## Baseline Scenario



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 300 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 194.7 |
| _P99_ | 197.1ms |
| _P95_ | 196.8ms |
| _P50_ | 195.4ms |
| _Tx validation time p50 (ms)_ | 89.1 |
| _End-to-end TPS_ | 1518.82 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.13 /s |
| _Avg txs per snapshot_ | 150.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Three local nodes



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 900 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 1066.7 |
| _P99_ | 1097.3ms |
| _P95_ | 1096.2ms |
| _P50_ | 1067.8ms |
| _Tx validation time p50 (ms)_ | 470.4 |
| _End-to-end TPS_ | 819.68 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 1.82 /s |
| _Avg txs per snapshot_ | 450.0 |
| _Peak node RSS (MB)_ | 172.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Plateau 1000 UTxO

Each client splits its funds into 1000 outputs (1-in 10-out), then holds that plateau with full-value self-transfers so every snapshot carries the large UTxO set.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 600 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 901.9 |
| _P99_ | 1014.6ms |
| _P95_ | 1014.2ms |
| _P50_ | 1006.0ms |
| _Tx validation time p50 (ms)_ | 239.2 |
| _End-to-end TPS_ | 585.88 tx/s |
| _Backlog drain time (s)_ | 1.0 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 2.93 /s |
| _Avg txs per snapshot_ | 200.0 |
| _Peak node RSS (MB)_ | 181.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1000 |
      
