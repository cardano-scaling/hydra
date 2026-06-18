--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-18 13:00:51.650567326 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 541.60 | 2392.12 | 54.6 | 55.1 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 180.64 | 188.44 | 5.5 | 6.5 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 450.77 | 1011.43 | 65.5 | 66.3 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 114.67 | 113.05 | 8.6 | 12.0 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 512.90 | 2255.56 | 57.7 | 58.2 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 130.17 | 129.99 | 7.6 | 9.7 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.1 | 424.88 | 2127.61 | 139.6 | 140.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 109.51 | 116.35 | 18.1 | 23.0 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 341.32 | 766.83 | 173.5 | 175.5 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 59.78 | 63.50 | 33.0 | 50.0 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 358.81 | 1052.15 | 165.0 | 166.9 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 74.82 | 77.97 | 26.4 | 37.2 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 334.93 | 1955.30 | 265.6 | 266.9 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 88.41 | 81.83 | 33.2 | 42.5 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 257.51 | 480.18 | 344.8 | 348.5 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.14 | 43.99 | 64.0 | 107.9 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 341.82 | 1500.48 | 260.8 | 263.0 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 60.89 | 58.60 | 48.6 | 72.9 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 54.6 |
| _P99_ | 55.1ms |
| _P95_ | 55.1ms |
| _P50_ | 54.7ms |
| _End-to-end TPS_ | 541.60 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2392.12 tx/s |
| _Per-snapshot TPS P95_ | 4526.77 tx/s |
| _Per-snapshot TPS max_ | 4716.51 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 6.6ms |
| _P95_ | 6.5ms |
| _P50_ | 5.3ms |
| _End-to-end TPS_ | 180.64 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 188.44 tx/s |
| _Per-snapshot TPS P95_ | 197.06 tx/s |
| _Per-snapshot TPS max_ | 201.68 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.5 |
| _P99_ | 66.3ms |
| _P95_ | 66.3ms |
| _P50_ | 66.0ms |
| _End-to-end TPS_ | 450.77 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1011.43 tx/s |
| _Per-snapshot TPS P95_ | 1904.43 tx/s |
| _Per-snapshot TPS max_ | 1983.80 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.6 |
| _P99_ | 12.8ms |
| _P95_ | 12.0ms |
| _P50_ | 8.7ms |
| _End-to-end TPS_ | 114.67 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 113.05 tx/s |
| _Per-snapshot TPS P95_ | 157.87 tx/s |
| _Per-snapshot TPS max_ | 171.52 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 57.7 |
| _P99_ | 58.2ms |
| _P95_ | 58.2ms |
| _P50_ | 57.9ms |
| _End-to-end TPS_ | 512.90 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2255.56 tx/s |
| _Per-snapshot TPS P95_ | 4268.27 tx/s |
| _Per-snapshot TPS max_ | 4447.17 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.6 |
| _P99_ | 10.2ms |
| _P95_ | 9.7ms |
| _P50_ | 7.6ms |
| _End-to-end TPS_ | 130.17 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 129.99 tx/s |
| _Per-snapshot TPS P95_ | 165.39 tx/s |
| _Per-snapshot TPS max_ | 168.87 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 139.6 |
| _P99_ | 141.0ms |
| _P95_ | 140.9ms |
| _P50_ | 139.8ms |
| _End-to-end TPS_ | 424.88 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2127.61 tx/s |
| _Per-snapshot TPS P95_ | 4035.34 tx/s |
| _Per-snapshot TPS max_ | 4204.92 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 28.2ms |
| _P95_ | 23.0ms |
| _P50_ | 17.5ms |
| _End-to-end TPS_ | 109.51 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 116.35 tx/s |
| _Per-snapshot TPS P95_ | 145.29 tx/s |
| _Per-snapshot TPS max_ | 154.68 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 173.5 |
| _P99_ | 175.5ms |
| _P95_ | 175.5ms |
| _P50_ | 174.1ms |
| _End-to-end TPS_ | 341.32 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 766.83 tx/s |
| _Per-snapshot TPS P95_ | 1450.36 tx/s |
| _Per-snapshot TPS max_ | 1511.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 57.6ms |
| _P95_ | 50.0ms |
| _P50_ | 32.5ms |
| _End-to-end TPS_ | 59.78 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 63.50 tx/s |
| _Per-snapshot TPS P95_ | 110.80 tx/s |
| _Per-snapshot TPS max_ | 129.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 165.0 |
| _P99_ | 167.0ms |
| _P95_ | 166.9ms |
| _P50_ | 165.4ms |
| _End-to-end TPS_ | 358.81 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1052.15 tx/s |
| _Per-snapshot TPS P95_ | 1992.53 tx/s |
| _Per-snapshot TPS max_ | 2076.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 26.4 |
| _P99_ | 39.2ms |
| _P95_ | 37.2ms |
| _P50_ | 26.0ms |
| _End-to-end TPS_ | 74.82 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 77.97 tx/s |
| _Per-snapshot TPS P95_ | 110.02 tx/s |
| _Per-snapshot TPS max_ | 129.41 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 265.6 |
| _P99_ | 267.2ms |
| _P95_ | 266.9ms |
| _P50_ | 265.8ms |
| _End-to-end TPS_ | 334.93 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1955.30 tx/s |
| _Per-snapshot TPS P95_ | 3711.39 tx/s |
| _Per-snapshot TPS max_ | 3867.49 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 48.5ms |
| _P95_ | 42.5ms |
| _P50_ | 31.7ms |
| _End-to-end TPS_ | 88.41 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 81.83 tx/s |
| _Per-snapshot TPS P95_ | 161.15 tx/s |
| _Per-snapshot TPS max_ | 200.27 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 344.8 |
| _P99_ | 348.6ms |
| _P95_ | 348.5ms |
| _P50_ | 347.3ms |
| _End-to-end TPS_ | 257.51 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 480.18 tx/s |
| _Per-snapshot TPS P95_ | 908.76 tx/s |
| _Per-snapshot TPS max_ | 946.86 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 64.0 |
| _P99_ | 112.6ms |
| _P95_ | 107.9ms |
| _P50_ | 61.3ms |
| _End-to-end TPS_ | 45.14 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 43.99 tx/s |
| _Per-snapshot TPS P95_ | 119.78 tx/s |
| _Per-snapshot TPS max_ | 154.18 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 260.8 |
| _P99_ | 263.0ms |
| _P95_ | 263.0ms |
| _P50_ | 260.9ms |
| _End-to-end TPS_ | 341.82 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1500.48 tx/s |
| _Per-snapshot TPS P95_ | 2847.04 tx/s |
| _Per-snapshot TPS max_ | 2966.73 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 48.6 |
| _P99_ | 91.1ms |
| _P95_ | 72.9ms |
| _P50_ | 47.1ms |
| _End-to-end TPS_ | 60.89 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 58.60 tx/s |
| _Per-snapshot TPS P95_ | 122.64 tx/s |
| _Per-snapshot TPS max_ | 143.13 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
