--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-17 13:35:57.766014612 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 513.47 | 2229.53 | 57.4 | 58.1 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 150.40 | 159.54 | 6.6 | 8.7 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 441.32 | 999.35 | 66.9 | 67.7 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 101.27 | 107.45 | 9.7 | 15.0 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 437.37 | 1950.64 | 67.8 | 68.4 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 122.51 | 126.63 | 8.0 | 10.9 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 328.11 | 1602.41 | 180.8 | 182.6 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 107.26 | 116.39 | 18.4 | 26.3 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 323.71 | 786.53 | 183.2 | 184.5 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 58.75 | 59.42 | 33.0 | 47.8 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 337.11 | 1741.42 | 176.4 | 177.7 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 75.32 | 79.54 | 26.3 | 35.0 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 333.04 | 1379.25 | 266.2 | 269.3 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 91.62 | 79.89 | 32.1 | 41.4 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 248.83 | 450.35 | 355.5 | 361.4 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.10 | 46.77 | 63.7 | 96.1 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 286.48 | 858.88 | 310.6 | 312.9 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 59.94 | 55.21 | 49.1 | 72.2 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 57.4 |
| _P99_ | 58.2ms |
| _P95_ | 58.1ms |
| _P50_ | 57.6ms |
| _End-to-end TPS_ | 513.47 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2229.53 tx/s |
| _Per-snapshot TPS P95_ | 4218.76 tx/s |
| _Per-snapshot TPS max_ | 4395.58 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.6 |
| _P99_ | 10.7ms |
| _P95_ | 8.7ms |
| _P50_ | 6.1ms |
| _End-to-end TPS_ | 150.40 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 159.54 tx/s |
| _Per-snapshot TPS P95_ | 179.91 tx/s |
| _Per-snapshot TPS max_ | 185.57 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 67.7ms |
| _P95_ | 67.7ms |
| _P50_ | 67.3ms |
| _End-to-end TPS_ | 441.32 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 999.35 tx/s |
| _Per-snapshot TPS P95_ | 1881.88 tx/s |
| _Per-snapshot TPS max_ | 1960.33 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 9.7 |
| _P99_ | 16.9ms |
| _P95_ | 15.0ms |
| _P50_ | 9.1ms |
| _End-to-end TPS_ | 101.27 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 107.45 tx/s |
| _Per-snapshot TPS P95_ | 152.58 tx/s |
| _Per-snapshot TPS max_ | 163.14 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 67.8 |
| _P99_ | 68.5ms |
| _P95_ | 68.4ms |
| _P50_ | 68.0ms |
| _End-to-end TPS_ | 437.37 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1950.64 tx/s |
| _Per-snapshot TPS P95_ | 3691.49 tx/s |
| _Per-snapshot TPS max_ | 3846.23 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.0 |
| _P99_ | 12.0ms |
| _P95_ | 10.9ms |
| _P50_ | 7.8ms |
| _End-to-end TPS_ | 122.51 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 126.63 tx/s |
| _Per-snapshot TPS P95_ | 166.75 tx/s |
| _Per-snapshot TPS max_ | 169.42 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 180.8 |
| _P99_ | 182.7ms |
| _P95_ | 182.6ms |
| _P50_ | 180.7ms |
| _End-to-end TPS_ | 328.11 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1602.41 tx/s |
| _Per-snapshot TPS P95_ | 3039.08 tx/s |
| _Per-snapshot TPS max_ | 3166.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 32.0ms |
| _P95_ | 26.3ms |
| _P50_ | 17.1ms |
| _End-to-end TPS_ | 107.26 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 116.39 tx/s |
| _Per-snapshot TPS P95_ | 144.92 tx/s |
| _Per-snapshot TPS max_ | 158.05 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 183.2 |
| _P99_ | 184.9ms |
| _P95_ | 184.5ms |
| _P50_ | 183.7ms |
| _End-to-end TPS_ | 323.71 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 786.53 tx/s |
| _Per-snapshot TPS P95_ | 1488.25 tx/s |
| _Per-snapshot TPS max_ | 1550.62 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 48.9ms |
| _P95_ | 47.8ms |
| _P50_ | 32.9ms |
| _End-to-end TPS_ | 58.75 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 59.42 tx/s |
| _Per-snapshot TPS P95_ | 97.04 tx/s |
| _Per-snapshot TPS max_ | 98.18 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 176.4 |
| _P99_ | 177.8ms |
| _P95_ | 177.7ms |
| _P50_ | 176.8ms |
| _End-to-end TPS_ | 337.11 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1741.42 tx/s |
| _Per-snapshot TPS P95_ | 3303.08 tx/s |
| _Per-snapshot TPS max_ | 3441.90 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 26.3 |
| _P99_ | 37.2ms |
| _P95_ | 35.0ms |
| _P50_ | 25.8ms |
| _End-to-end TPS_ | 75.32 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 79.54 tx/s |
| _Per-snapshot TPS P95_ | 107.68 tx/s |
| _Per-snapshot TPS max_ | 143.42 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 266.2 |
| _P99_ | 269.5ms |
| _P95_ | 269.3ms |
| _P50_ | 266.2ms |
| _End-to-end TPS_ | 333.04 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1379.25 tx/s |
| _Per-snapshot TPS P95_ | 2616.76 tx/s |
| _Per-snapshot TPS max_ | 2726.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 32.1 |
| _P99_ | 44.2ms |
| _P95_ | 41.4ms |
| _P50_ | 31.9ms |
| _End-to-end TPS_ | 91.62 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 79.89 tx/s |
| _Per-snapshot TPS P95_ | 174.21 tx/s |
| _Per-snapshot TPS max_ | 194.60 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 355.5 |
| _P99_ | 361.6ms |
| _P95_ | 361.4ms |
| _P50_ | 354.9ms |
| _End-to-end TPS_ | 248.83 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 450.35 tx/s |
| _Per-snapshot TPS P95_ | 852.18 tx/s |
| _Per-snapshot TPS max_ | 887.89 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 63.7 |
| _P99_ | 108.8ms |
| _P95_ | 96.1ms |
| _P50_ | 62.7ms |
| _End-to-end TPS_ | 45.10 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 46.77 tx/s |
| _Per-snapshot TPS P95_ | 108.31 tx/s |
| _Per-snapshot TPS max_ | 152.63 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 310.6 |
| _P99_ | 312.9ms |
| _P95_ | 312.9ms |
| _P50_ | 312.5ms |
| _End-to-end TPS_ | 286.48 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 858.88 tx/s |
| _Per-snapshot TPS P95_ | 1628.41 tx/s |
| _Per-snapshot TPS max_ | 1696.81 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 49.1 |
| _P99_ | 79.2ms |
| _P95_ | 72.2ms |
| _P50_ | 48.0ms |
| _End-to-end TPS_ | 59.94 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 55.21 tx/s |
| _Per-snapshot TPS P95_ | 120.57 tx/s |
| _Per-snapshot TPS max_ | 130.07 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
