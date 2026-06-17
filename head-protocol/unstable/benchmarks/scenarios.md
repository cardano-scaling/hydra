--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-17 16:19:03.309169993 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 541.56 | 2557.26 | 54.6 | 55.1 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 164.17 | 172.42 | 6.0 | 7.4 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 467.59 | 1030.04 | 63.1 | 63.9 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 110.50 | 111.36 | 8.9 | 11.5 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 502.15 | 2321.89 | 59.0 | 59.5 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 135.09 | 134.49 | 7.3 | 8.9 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.1 | 414.63 | 2601.00 | 143.5 | 143.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 104.88 | 112.53 | 18.8 | 27.6 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 281.27 | 730.67 | 210.6 | 212.1 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 59.03 | 62.49 | 32.9 | 49.9 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 392.26 | 1757.09 | 151.1 | 152.7 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 77.38 | 79.33 | 25.4 | 34.8 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 359.74 | 1947.73 | 246.1 | 249.8 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 87.48 | 80.87 | 33.8 | 46.9 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 214.60 | 422.26 | 411.8 | 418.8 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.89 | 42.70 | 67.0 | 106.3 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 291.22 | 1053.02 | 304.7 | 308.7 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 58.60 | 55.10 | 50.0 | 65.8 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 54.6 |
| _P99_ | 55.1ms |
| _P95_ | 55.1ms |
| _P50_ | 54.8ms |
| _End-to-end TPS_ | 541.56 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2557.26 tx/s |
| _Per-snapshot TPS P95_ | 4840.68 tx/s |
| _Per-snapshot TPS max_ | 5043.65 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.2ms |
| _P95_ | 7.4ms |
| _P50_ | 5.7ms |
| _End-to-end TPS_ | 164.17 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 172.42 tx/s |
| _Per-snapshot TPS P95_ | 188.10 tx/s |
| _Per-snapshot TPS max_ | 190.03 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 63.1 |
| _P99_ | 63.9ms |
| _P95_ | 63.9ms |
| _P50_ | 63.6ms |
| _End-to-end TPS_ | 467.59 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1030.04 tx/s |
| _Per-snapshot TPS P95_ | 1939.04 tx/s |
| _Per-snapshot TPS max_ | 2019.84 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.9 |
| _P99_ | 11.9ms |
| _P95_ | 11.5ms |
| _P50_ | 8.8ms |
| _End-to-end TPS_ | 110.50 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 111.36 tx/s |
| _Per-snapshot TPS P95_ | 158.64 tx/s |
| _Per-snapshot TPS max_ | 169.55 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 59.0 |
| _P99_ | 59.5ms |
| _P95_ | 59.5ms |
| _P50_ | 59.1ms |
| _End-to-end TPS_ | 502.15 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2321.89 tx/s |
| _Per-snapshot TPS P95_ | 4394.76 tx/s |
| _Per-snapshot TPS max_ | 4579.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.3 |
| _P99_ | 9.9ms |
| _P95_ | 8.9ms |
| _P50_ | 7.3ms |
| _End-to-end TPS_ | 135.09 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 134.49 tx/s |
| _Per-snapshot TPS P95_ | 163.92 tx/s |
| _Per-snapshot TPS max_ | 175.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 143.5 |
| _P99_ | 144.0ms |
| _P95_ | 143.9ms |
| _P50_ | 143.7ms |
| _End-to-end TPS_ | 414.63 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2601.00 tx/s |
| _Per-snapshot TPS P95_ | 4935.12 tx/s |
| _Per-snapshot TPS max_ | 5142.59 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.8 |
| _P99_ | 30.3ms |
| _P95_ | 27.6ms |
| _P50_ | 17.9ms |
| _End-to-end TPS_ | 104.88 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 112.53 tx/s |
| _Per-snapshot TPS P95_ | 148.24 tx/s |
| _Per-snapshot TPS max_ | 155.15 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 210.6 |
| _P99_ | 212.2ms |
| _P95_ | 212.1ms |
| _P50_ | 211.3ms |
| _End-to-end TPS_ | 281.27 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 730.67 tx/s |
| _Per-snapshot TPS P95_ | 1383.03 tx/s |
| _Per-snapshot TPS max_ | 1441.01 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 54.0ms |
| _P95_ | 49.9ms |
| _P50_ | 32.3ms |
| _End-to-end TPS_ | 59.03 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 62.49 tx/s |
| _Per-snapshot TPS P95_ | 103.33 tx/s |
| _Per-snapshot TPS max_ | 107.26 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 151.1 |
| _P99_ | 152.7ms |
| _P95_ | 152.7ms |
| _P50_ | 151.4ms |
| _End-to-end TPS_ | 392.26 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1757.09 tx/s |
| _Per-snapshot TPS P95_ | 3331.81 tx/s |
| _Per-snapshot TPS max_ | 3471.79 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 25.4 |
| _P99_ | 38.1ms |
| _P95_ | 34.8ms |
| _P50_ | 24.8ms |
| _End-to-end TPS_ | 77.38 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 79.33 tx/s |
| _Per-snapshot TPS P95_ | 126.12 tx/s |
| _Per-snapshot TPS max_ | 126.84 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 246.1 |
| _P99_ | 249.9ms |
| _P95_ | 249.8ms |
| _P50_ | 246.7ms |
| _End-to-end TPS_ | 359.74 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1947.73 tx/s |
| _Per-snapshot TPS P95_ | 3696.67 tx/s |
| _Per-snapshot TPS max_ | 3852.13 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.8 |
| _P99_ | 51.5ms |
| _P95_ | 46.9ms |
| _P50_ | 32.4ms |
| _End-to-end TPS_ | 87.48 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 80.87 tx/s |
| _Per-snapshot TPS P95_ | 159.55 tx/s |
| _Per-snapshot TPS max_ | 186.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 411.8 |
| _P99_ | 419.0ms |
| _P95_ | 418.8ms |
| _P50_ | 411.8ms |
| _End-to-end TPS_ | 214.60 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 422.26 tx/s |
| _Per-snapshot TPS P95_ | 799.40 tx/s |
| _Per-snapshot TPS max_ | 832.93 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 67.0 |
| _P99_ | 110.6ms |
| _P95_ | 106.3ms |
| _P50_ | 64.4ms |
| _End-to-end TPS_ | 43.89 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 42.70 tx/s |
| _Per-snapshot TPS P95_ | 94.92 tx/s |
| _Per-snapshot TPS max_ | 145.72 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 304.7 |
| _P99_ | 308.8ms |
| _P95_ | 308.7ms |
| _P50_ | 303.9ms |
| _End-to-end TPS_ | 291.22 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1053.02 tx/s |
| _Per-snapshot TPS P95_ | 1997.30 tx/s |
| _Per-snapshot TPS max_ | 2081.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 50.0 |
| _P99_ | 84.2ms |
| _P95_ | 65.8ms |
| _P50_ | 51.3ms |
| _End-to-end TPS_ | 58.60 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 55.10 tx/s |
| _Per-snapshot TPS P95_ | 109.23 tx/s |
| _Per-snapshot TPS max_ | 148.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
