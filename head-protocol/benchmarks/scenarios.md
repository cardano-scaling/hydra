--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-12 15:19:46.015681192 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 493.47 | 1812.38 | 59.9 | 60.5 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 172.22 | 183.56 | 5.7 | 7.0 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 417.87 | 925.24 | 70.5 | 71.5 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 120.76 | 123.81 | 8.2 | 10.9 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 467.46 | 1687.24 | 63.2 | 63.9 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 139.30 | 145.69 | 7.1 | 9.6 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 372.23 | 1139.94 | 158.7 | 160.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 113.78 | 118.13 | 17.4 | 21.6 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 326.72 | 680.60 | 181.0 | 183.4 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 62.66 | 64.45 | 31.3 | 44.4 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 324.28 | 1187.93 | 182.4 | 184.6 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.7 | 82.94 | 87.00 | 23.9 | 31.5 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 301.86 | 1062.24 | 293.7 | 297.1 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 94.29 | 82.65 | 31.3 | 37.2 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 244.62 | 359.88 | 358.6 | 363.2 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.51 | 43.16 | 64.1 | 95.6 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 279.81 | 783.40 | 317.2 | 318.6 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 61.10 | 53.47 | 47.9 | 70.4 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 59.9 |
| _P99_ | 60.6ms |
| _P95_ | 60.5ms |
| _P50_ | 60.2ms |
| _End-to-end TPS_ | 493.47 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1812.38 tx/s |
| _Per-snapshot TPS P95_ | 3426.45 tx/s |
| _Per-snapshot TPS max_ | 3569.92 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 8.9ms |
| _P95_ | 7.0ms |
| _P50_ | 5.4ms |
| _End-to-end TPS_ | 172.22 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 183.56 tx/s |
| _Per-snapshot TPS P95_ | 196.12 tx/s |
| _Per-snapshot TPS max_ | 202.46 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 70.5 |
| _P99_ | 71.5ms |
| _P95_ | 71.5ms |
| _P50_ | 71.0ms |
| _End-to-end TPS_ | 417.87 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 925.24 tx/s |
| _Per-snapshot TPS P95_ | 1741.87 tx/s |
| _Per-snapshot TPS max_ | 1814.46 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.2 |
| _P99_ | 12.3ms |
| _P95_ | 10.9ms |
| _P50_ | 8.0ms |
| _End-to-end TPS_ | 120.76 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 123.81 tx/s |
| _Per-snapshot TPS P95_ | 165.73 tx/s |
| _Per-snapshot TPS max_ | 179.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 63.2 |
| _P99_ | 64.0ms |
| _P95_ | 63.9ms |
| _P50_ | 63.4ms |
| _End-to-end TPS_ | 467.46 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1687.24 tx/s |
| _Per-snapshot TPS P95_ | 3189.54 tx/s |
| _Per-snapshot TPS max_ | 3323.08 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.1 |
| _P99_ | 10.5ms |
| _P95_ | 9.6ms |
| _P50_ | 6.8ms |
| _End-to-end TPS_ | 139.30 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 145.69 tx/s |
| _Per-snapshot TPS P95_ | 179.51 tx/s |
| _Per-snapshot TPS max_ | 189.69 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 158.7 |
| _P99_ | 161.0ms |
| _P95_ | 160.9ms |
| _P50_ | 158.0ms |
| _End-to-end TPS_ | 372.23 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1139.94 tx/s |
| _Per-snapshot TPS P95_ | 2159.11 tx/s |
| _Per-snapshot TPS max_ | 2249.71 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 17.4 |
| _P99_ | 23.5ms |
| _P95_ | 21.6ms |
| _P50_ | 17.1ms |
| _End-to-end TPS_ | 113.78 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 118.13 tx/s |
| _Per-snapshot TPS P95_ | 145.95 tx/s |
| _Per-snapshot TPS max_ | 168.37 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 181.0 |
| _P99_ | 183.5ms |
| _P95_ | 183.4ms |
| _P50_ | 181.8ms |
| _End-to-end TPS_ | 326.72 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 680.60 tx/s |
| _Per-snapshot TPS P95_ | 1286.68 tx/s |
| _Per-snapshot TPS max_ | 1340.56 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 49.6ms |
| _P95_ | 44.4ms |
| _P50_ | 30.7ms |
| _End-to-end TPS_ | 62.66 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 64.45 tx/s |
| _Per-snapshot TPS P95_ | 103.72 tx/s |
| _Per-snapshot TPS max_ | 122.35 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 182.4 |
| _P99_ | 184.8ms |
| _P95_ | 184.6ms |
| _P50_ | 183.3ms |
| _End-to-end TPS_ | 324.28 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1187.93 tx/s |
| _Per-snapshot TPS P95_ | 2251.43 tx/s |
| _Per-snapshot TPS max_ | 2345.96 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 23.9 |
| _P99_ | 34.6ms |
| _P95_ | 31.5ms |
| _P50_ | 23.0ms |
| _End-to-end TPS_ | 82.94 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 87.00 tx/s |
| _Per-snapshot TPS P95_ | 117.47 tx/s |
| _Per-snapshot TPS max_ | 152.82 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 293.7 |
| _P99_ | 297.3ms |
| _P95_ | 297.1ms |
| _P50_ | 293.0ms |
| _End-to-end TPS_ | 301.86 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1062.24 tx/s |
| _Per-snapshot TPS P95_ | 2014.71 tx/s |
| _Per-snapshot TPS max_ | 2099.38 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 41.7ms |
| _P95_ | 37.2ms |
| _P50_ | 32.0ms |
| _End-to-end TPS_ | 94.29 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 82.65 tx/s |
| _Per-snapshot TPS P95_ | 170.47 tx/s |
| _Per-snapshot TPS max_ | 218.37 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 358.6 |
| _P99_ | 363.8ms |
| _P95_ | 363.2ms |
| _P50_ | 358.7ms |
| _End-to-end TPS_ | 244.62 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 359.88 tx/s |
| _Per-snapshot TPS P95_ | 679.95 tx/s |
| _Per-snapshot TPS max_ | 708.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 64.1 |
| _P99_ | 107.5ms |
| _P95_ | 95.6ms |
| _P50_ | 63.8ms |
| _End-to-end TPS_ | 45.51 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 43.16 tx/s |
| _Per-snapshot TPS P95_ | 116.98 tx/s |
| _Per-snapshot TPS max_ | 141.67 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 317.2 |
| _P99_ | 319.0ms |
| _P95_ | 318.6ms |
| _P50_ | 317.9ms |
| _End-to-end TPS_ | 279.81 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 783.40 tx/s |
| _Per-snapshot TPS P95_ | 1485.02 tx/s |
| _Per-snapshot TPS max_ | 1547.39 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 47.9 |
| _P99_ | 81.5ms |
| _P95_ | 70.4ms |
| _P50_ | 47.4ms |
| _End-to-end TPS_ | 61.10 tx/s |
| _Snapshots observed_ | 64 |
| _Per-snapshot TPS P50_ | 53.47 tx/s |
| _Per-snapshot TPS P95_ | 116.17 tx/s |
| _Per-snapshot TPS max_ | 158.13 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
