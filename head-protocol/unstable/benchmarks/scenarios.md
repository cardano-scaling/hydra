--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-16 13:36:10.727355141 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 473.09 | 1639.18 | 62.5 | 63.1 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 147.31 | 159.47 | 6.7 | 9.0 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 396.46 | 819.41 | 74.3 | 75.4 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 103.05 | 102.06 | 9.5 | 12.1 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 452.43 | 1593.29 | 65.5 | 66.0 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 131.40 | 139.79 | 7.5 | 10.4 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 371.46 | 1194.31 | 159.9 | 160.5 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 107.26 | 115.33 | 18.3 | 23.4 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 299.21 | 513.73 | 198.0 | 200.3 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.1 | 56.49 | 57.55 | 34.3 | 51.0 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 301.51 | 1253.54 | 197.3 | 198.0 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 73.62 | 79.05 | 26.9 | 38.4 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 289.12 | 1157.24 | 308.1 | 310.4 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 86.92 | 72.95 | 33.9 | 43.2 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 233.77 | 421.20 | 374.2 | 383.3 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 42.69 | 42.69 | 66.7 | 106.4 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 282.12 | 951.46 | 314.4 | 316.8 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.6 | 57.21 | 50.14 | 51.4 | 73.6 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 62.5 |
| _P99_ | 63.1ms |
| _P95_ | 63.1ms |
| _P50_ | 62.8ms |
| _End-to-end TPS_ | 473.09 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1639.18 tx/s |
| _Per-snapshot TPS P95_ | 3097.93 tx/s |
| _Per-snapshot TPS max_ | 3227.60 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.7 |
| _P99_ | 13.7ms |
| _P95_ | 9.0ms |
| _P50_ | 6.2ms |
| _End-to-end TPS_ | 147.31 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 159.47 tx/s |
| _Per-snapshot TPS P95_ | 177.33 tx/s |
| _Per-snapshot TPS max_ | 178.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 74.3 |
| _P99_ | 75.4ms |
| _P95_ | 75.4ms |
| _P50_ | 74.9ms |
| _End-to-end TPS_ | 396.46 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 819.41 tx/s |
| _Per-snapshot TPS P95_ | 1541.30 tx/s |
| _Per-snapshot TPS max_ | 1605.46 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 9.5 |
| _P99_ | 12.9ms |
| _P95_ | 12.1ms |
| _P50_ | 9.6ms |
| _End-to-end TPS_ | 103.05 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 102.06 tx/s |
| _Per-snapshot TPS P95_ | 135.09 tx/s |
| _Per-snapshot TPS max_ | 149.84 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.5 |
| _P99_ | 66.1ms |
| _P95_ | 66.0ms |
| _P50_ | 65.8ms |
| _End-to-end TPS_ | 452.43 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1593.29 tx/s |
| _Per-snapshot TPS P95_ | 3011.50 tx/s |
| _Per-snapshot TPS max_ | 3137.56 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.5 |
| _P99_ | 12.4ms |
| _P95_ | 10.4ms |
| _P50_ | 7.1ms |
| _End-to-end TPS_ | 131.40 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 139.79 tx/s |
| _Per-snapshot TPS P95_ | 169.48 tx/s |
| _Per-snapshot TPS max_ | 176.44 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 159.9 |
| _P99_ | 160.9ms |
| _P95_ | 160.5ms |
| _P50_ | 160.3ms |
| _End-to-end TPS_ | 371.46 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1194.31 tx/s |
| _Per-snapshot TPS P95_ | 2262.56 tx/s |
| _Per-snapshot TPS max_ | 2357.51 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.3 |
| _P99_ | 27.9ms |
| _P95_ | 23.4ms |
| _P50_ | 17.7ms |
| _End-to-end TPS_ | 107.26 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 115.33 tx/s |
| _Per-snapshot TPS P95_ | 141.81 tx/s |
| _Per-snapshot TPS max_ | 154.29 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 198.0 |
| _P99_ | 200.3ms |
| _P95_ | 200.3ms |
| _P50_ | 199.0ms |
| _End-to-end TPS_ | 299.21 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 513.73 tx/s |
| _Per-snapshot TPS P95_ | 969.74 tx/s |
| _Per-snapshot TPS max_ | 1010.27 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 34.3 |
| _P99_ | 59.4ms |
| _P95_ | 51.0ms |
| _P50_ | 33.5ms |
| _End-to-end TPS_ | 56.49 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 57.55 tx/s |
| _Per-snapshot TPS P95_ | 95.25 tx/s |
| _Per-snapshot TPS max_ | 111.29 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 197.3 |
| _P99_ | 198.4ms |
| _P95_ | 198.0ms |
| _P50_ | 197.7ms |
| _End-to-end TPS_ | 301.51 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1253.54 tx/s |
| _Per-snapshot TPS P95_ | 2376.57 tx/s |
| _Per-snapshot TPS max_ | 2476.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 26.9 |
| _P99_ | 42.0ms |
| _P95_ | 38.4ms |
| _P50_ | 26.1ms |
| _End-to-end TPS_ | 73.62 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 79.05 tx/s |
| _Per-snapshot TPS P95_ | 105.00 tx/s |
| _Per-snapshot TPS max_ | 121.68 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 308.1 |
| _P99_ | 310.5ms |
| _P95_ | 310.4ms |
| _P50_ | 308.3ms |
| _End-to-end TPS_ | 289.12 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1157.24 tx/s |
| _Per-snapshot TPS P95_ | 2195.44 tx/s |
| _Per-snapshot TPS max_ | 2287.72 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 51.3ms |
| _P95_ | 43.2ms |
| _P50_ | 33.0ms |
| _End-to-end TPS_ | 86.92 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 72.95 tx/s |
| _Per-snapshot TPS P95_ | 169.43 tx/s |
| _Per-snapshot TPS max_ | 180.94 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 374.2 |
| _P99_ | 383.4ms |
| _P95_ | 383.3ms |
| _P50_ | 372.0ms |
| _End-to-end TPS_ | 233.77 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 421.20 tx/s |
| _Per-snapshot TPS P95_ | 796.89 tx/s |
| _Per-snapshot TPS max_ | 830.29 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 66.7 |
| _P99_ | 124.0ms |
| _P95_ | 106.4ms |
| _P50_ | 64.3ms |
| _End-to-end TPS_ | 42.69 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 42.69 tx/s |
| _Per-snapshot TPS P95_ | 97.90 tx/s |
| _Per-snapshot TPS max_ | 147.81 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 314.4 |
| _P99_ | 317.1ms |
| _P95_ | 316.8ms |
| _P50_ | 314.2ms |
| _End-to-end TPS_ | 282.12 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 951.46 tx/s |
| _Per-snapshot TPS P95_ | 1804.42 tx/s |
| _Per-snapshot TPS max_ | 1880.24 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 51.4 |
| _P99_ | 79.6ms |
| _P95_ | 73.6ms |
| _P50_ | 50.2ms |
| _End-to-end TPS_ | 57.21 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 50.14 tx/s |
| _Per-snapshot TPS P95_ | 122.89 tx/s |
| _Per-snapshot TPS max_ | 132.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
