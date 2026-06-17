--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-17 14:58:45.655663825 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 470.31 | 1851.39 | 62.9 | 63.5 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 168.20 | 174.57 | 5.9 | 7.0 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 396.89 | 821.73 | 74.3 | 75.3 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 114.60 | 118.50 | 8.6 | 11.2 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 507.07 | 2134.38 | 58.4 | 59.0 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 140.70 | 147.44 | 7.0 | 9.6 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 387.05 | 1629.74 | 153.2 | 154.0 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 108.47 | 112.22 | 18.0 | 24.0 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 302.65 | 653.86 | 195.5 | 196.9 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 61.52 | 62.89 | 31.8 | 46.9 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 394.52 | 1295.89 | 150.3 | 151.3 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 76.33 | 78.68 | 25.9 | 33.6 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 348.13 | 1536.16 | 254.8 | 257.9 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.1 | 84.56 | 70.45 | 34.7 | 44.7 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 274.16 | 453.16 | 323.1 | 327.0 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.02 | 46.67 | 65.1 | 95.2 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 292.48 | 981.97 | 304.8 | 307.4 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 60.30 | 58.01 | 49.1 | 67.4 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 62.9 |
| _P99_ | 63.6ms |
| _P95_ | 63.5ms |
| _P50_ | 63.1ms |
| _End-to-end TPS_ | 470.31 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1851.39 tx/s |
| _Per-snapshot TPS P95_ | 3501.54 tx/s |
| _Per-snapshot TPS max_ | 3648.21 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 7.9ms |
| _P95_ | 7.0ms |
| _P50_ | 5.7ms |
| _End-to-end TPS_ | 168.20 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 174.57 tx/s |
| _Per-snapshot TPS P95_ | 194.34 tx/s |
| _Per-snapshot TPS max_ | 195.81 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 74.3 |
| _P99_ | 75.4ms |
| _P95_ | 75.3ms |
| _P50_ | 74.8ms |
| _End-to-end TPS_ | 396.89 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 821.73 tx/s |
| _Per-snapshot TPS P95_ | 1545.70 tx/s |
| _Per-snapshot TPS max_ | 1610.05 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.6 |
| _P99_ | 12.2ms |
| _P95_ | 11.2ms |
| _P50_ | 8.3ms |
| _End-to-end TPS_ | 114.60 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 118.50 tx/s |
| _Per-snapshot TPS P95_ | 152.22 tx/s |
| _Per-snapshot TPS max_ | 164.07 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 58.4 |
| _P99_ | 59.0ms |
| _P95_ | 59.0ms |
| _P50_ | 58.6ms |
| _End-to-end TPS_ | 507.07 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2134.38 tx/s |
| _Per-snapshot TPS P95_ | 4038.13 tx/s |
| _Per-snapshot TPS max_ | 4207.35 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.0 |
| _P99_ | 10.4ms |
| _P95_ | 9.6ms |
| _P50_ | 6.7ms |
| _End-to-end TPS_ | 140.70 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 147.44 tx/s |
| _Per-snapshot TPS P95_ | 180.96 tx/s |
| _Per-snapshot TPS max_ | 196.11 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 153.2 |
| _P99_ | 154.1ms |
| _P95_ | 154.0ms |
| _P50_ | 153.5ms |
| _End-to-end TPS_ | 387.05 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1629.74 tx/s |
| _Per-snapshot TPS P95_ | 3089.90 tx/s |
| _Per-snapshot TPS max_ | 3219.69 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.0 |
| _P99_ | 29.4ms |
| _P95_ | 24.0ms |
| _P50_ | 17.7ms |
| _End-to-end TPS_ | 108.47 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 112.22 tx/s |
| _Per-snapshot TPS P95_ | 146.90 tx/s |
| _Per-snapshot TPS max_ | 152.62 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 195.5 |
| _P99_ | 197.0ms |
| _P95_ | 196.9ms |
| _P50_ | 196.2ms |
| _End-to-end TPS_ | 302.65 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 653.86 tx/s |
| _Per-snapshot TPS P95_ | 1236.40 tx/s |
| _Per-snapshot TPS max_ | 1288.18 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 31.8 |
| _P99_ | 53.1ms |
| _P95_ | 46.9ms |
| _P50_ | 31.0ms |
| _End-to-end TPS_ | 61.52 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 62.89 tx/s |
| _Per-snapshot TPS P95_ | 112.88 tx/s |
| _Per-snapshot TPS max_ | 121.53 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 150.3 |
| _P99_ | 151.4ms |
| _P95_ | 151.3ms |
| _P50_ | 150.6ms |
| _End-to-end TPS_ | 394.52 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1295.89 tx/s |
| _Per-snapshot TPS P95_ | 2455.20 tx/s |
| _Per-snapshot TPS max_ | 2558.25 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 25.9 |
| _P99_ | 38.8ms |
| _P95_ | 33.6ms |
| _P50_ | 25.8ms |
| _End-to-end TPS_ | 76.33 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 78.68 tx/s |
| _Per-snapshot TPS P95_ | 116.90 tx/s |
| _Per-snapshot TPS max_ | 153.16 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 254.8 |
| _P99_ | 258.0ms |
| _P95_ | 257.9ms |
| _P50_ | 255.2ms |
| _End-to-end TPS_ | 348.13 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1536.16 tx/s |
| _Per-snapshot TPS P95_ | 2914.72 tx/s |
| _Per-snapshot TPS max_ | 3037.26 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 34.7 |
| _P99_ | 48.9ms |
| _P95_ | 44.7ms |
| _P50_ | 34.4ms |
| _End-to-end TPS_ | 84.56 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 70.45 tx/s |
| _Per-snapshot TPS P95_ | 163.67 tx/s |
| _Per-snapshot TPS max_ | 169.41 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 323.1 |
| _P99_ | 327.3ms |
| _P95_ | 327.0ms |
| _P50_ | 323.7ms |
| _End-to-end TPS_ | 274.16 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 453.16 tx/s |
| _Per-snapshot TPS P95_ | 857.01 tx/s |
| _Per-snapshot TPS max_ | 892.91 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 65.1 |
| _P99_ | 103.4ms |
| _P95_ | 95.2ms |
| _P50_ | 64.9ms |
| _End-to-end TPS_ | 45.02 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 46.67 tx/s |
| _Per-snapshot TPS P95_ | 114.36 tx/s |
| _Per-snapshot TPS max_ | 137.62 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 304.8 |
| _P99_ | 307.5ms |
| _P95_ | 307.4ms |
| _P50_ | 305.3ms |
| _End-to-end TPS_ | 292.48 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 981.97 tx/s |
| _Per-snapshot TPS P95_ | 1862.31 tx/s |
| _Per-snapshot TPS max_ | 1940.56 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 49.1 |
| _P99_ | 73.3ms |
| _P95_ | 67.4ms |
| _P50_ | 49.4ms |
| _End-to-end TPS_ | 60.30 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 58.01 tx/s |
| _Per-snapshot TPS P95_ | 125.23 tx/s |
| _Per-snapshot TPS max_ | 142.79 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
