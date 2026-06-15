--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-15 09:31:21.116047122 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 505.80 | 1779.85 | 58.4 | 59.0 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 174.83 | 183.37 | 5.7 | 6.9 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 439.98 | 884.49 | 67.1 | 67.9 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 115.59 | 118.78 | 8.5 | 10.9 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 487.92 | 1639.14 | 60.7 | 61.2 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 136.11 | 136.85 | 7.2 | 8.7 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 355.51 | 1234.06 | 166.9 | 168.6 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 112.58 | 116.80 | 17.6 | 22.9 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 328.31 | 567.47 | 180.6 | 181.8 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 60.88 | 59.95 | 32.2 | 44.7 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 365.78 | 1353.51 | 162.0 | 163.0 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 76.55 | 82.63 | 25.8 | 36.1 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 309.17 | 1053.20 | 287.2 | 289.5 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 89.69 | 77.94 | 33.1 | 45.3 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 212.79 | 326.20 | 416.0 | 419.3 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.40 | 45.76 | 67.1 | 102.0 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 288.96 | 837.09 | 308.3 | 309.4 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 60.62 | 56.64 | 48.6 | 71.7 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 58.4 |
| _P99_ | 59.1ms |
| _P95_ | 59.0ms |
| _P50_ | 58.7ms |
| _End-to-end TPS_ | 505.80 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1779.85 tx/s |
| _Per-snapshot TPS P95_ | 3364.11 tx/s |
| _Per-snapshot TPS max_ | 3504.93 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.2ms |
| _P95_ | 6.9ms |
| _P50_ | 5.4ms |
| _End-to-end TPS_ | 174.83 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 183.37 tx/s |
| _Per-snapshot TPS P95_ | 194.47 tx/s |
| _Per-snapshot TPS max_ | 197.21 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 67.1 |
| _P99_ | 67.9ms |
| _P95_ | 67.9ms |
| _P50_ | 67.6ms |
| _End-to-end TPS_ | 439.98 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 884.49 tx/s |
| _Per-snapshot TPS P95_ | 1663.09 tx/s |
| _Per-snapshot TPS max_ | 1732.30 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.5 |
| _P99_ | 12.4ms |
| _P95_ | 10.9ms |
| _P50_ | 8.3ms |
| _End-to-end TPS_ | 115.59 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 118.78 tx/s |
| _Per-snapshot TPS P95_ | 156.18 tx/s |
| _Per-snapshot TPS max_ | 159.75 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 60.7 |
| _P99_ | 61.2ms |
| _P95_ | 61.2ms |
| _P50_ | 60.9ms |
| _End-to-end TPS_ | 487.92 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1639.14 tx/s |
| _Per-snapshot TPS P95_ | 3097.25 tx/s |
| _Per-snapshot TPS max_ | 3226.86 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.2 |
| _P99_ | 10.0ms |
| _P95_ | 8.7ms |
| _P50_ | 7.2ms |
| _End-to-end TPS_ | 136.11 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 136.85 tx/s |
| _Per-snapshot TPS P95_ | 169.94 tx/s |
| _Per-snapshot TPS max_ | 173.58 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 166.9 |
| _P99_ | 168.6ms |
| _P95_ | 168.6ms |
| _P50_ | 166.5ms |
| _End-to-end TPS_ | 355.51 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1234.06 tx/s |
| _Per-snapshot TPS P95_ | 2338.44 tx/s |
| _Per-snapshot TPS max_ | 2436.61 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 17.6 |
| _P99_ | 23.8ms |
| _P95_ | 22.9ms |
| _P50_ | 17.0ms |
| _End-to-end TPS_ | 112.58 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 116.80 tx/s |
| _Per-snapshot TPS P95_ | 149.35 tx/s |
| _Per-snapshot TPS max_ | 162.42 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 180.6 |
| _P99_ | 181.9ms |
| _P95_ | 181.8ms |
| _P50_ | 181.5ms |
| _End-to-end TPS_ | 328.31 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 567.47 tx/s |
| _Per-snapshot TPS P95_ | 1071.24 tx/s |
| _Per-snapshot TPS max_ | 1116.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 32.2 |
| _P99_ | 47.5ms |
| _P95_ | 44.7ms |
| _P50_ | 32.1ms |
| _End-to-end TPS_ | 60.88 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 59.95 tx/s |
| _Per-snapshot TPS P95_ | 107.05 tx/s |
| _Per-snapshot TPS max_ | 124.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 162.0 |
| _P99_ | 163.1ms |
| _P95_ | 163.0ms |
| _P50_ | 162.4ms |
| _End-to-end TPS_ | 365.78 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1353.51 tx/s |
| _Per-snapshot TPS P95_ | 2565.30 tx/s |
| _Per-snapshot TPS max_ | 2673.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 25.8 |
| _P99_ | 39.9ms |
| _P95_ | 36.1ms |
| _P50_ | 25.1ms |
| _End-to-end TPS_ | 76.55 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 82.63 tx/s |
| _Per-snapshot TPS P95_ | 107.50 tx/s |
| _Per-snapshot TPS max_ | 140.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 287.2 |
| _P99_ | 289.7ms |
| _P95_ | 289.5ms |
| _P50_ | 289.1ms |
| _End-to-end TPS_ | 309.17 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1053.20 tx/s |
| _Per-snapshot TPS P95_ | 1997.44 tx/s |
| _Per-snapshot TPS max_ | 2081.37 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.1 |
| _P99_ | 51.4ms |
| _P95_ | 45.3ms |
| _P50_ | 32.0ms |
| _End-to-end TPS_ | 89.69 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 77.94 tx/s |
| _Per-snapshot TPS P95_ | 170.67 tx/s |
| _Per-snapshot TPS max_ | 184.61 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 416.0 |
| _P99_ | 420.0ms |
| _P95_ | 419.3ms |
| _P50_ | 417.9ms |
| _End-to-end TPS_ | 212.79 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 326.20 tx/s |
| _Per-snapshot TPS P95_ | 616.59 tx/s |
| _Per-snapshot TPS max_ | 642.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 67.1 |
| _P99_ | 127.6ms |
| _P95_ | 102.0ms |
| _P50_ | 66.1ms |
| _End-to-end TPS_ | 43.40 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 45.76 tx/s |
| _Per-snapshot TPS P95_ | 103.83 tx/s |
| _Per-snapshot TPS max_ | 133.35 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 308.3 |
| _P99_ | 309.4ms |
| _P95_ | 309.4ms |
| _P50_ | 308.9ms |
| _End-to-end TPS_ | 288.96 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 837.09 tx/s |
| _Per-snapshot TPS P95_ | 1586.95 tx/s |
| _Per-snapshot TPS max_ | 1653.61 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 48.6 |
| _P99_ | 75.9ms |
| _P95_ | 71.7ms |
| _P50_ | 47.5ms |
| _End-to-end TPS_ | 60.62 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 56.64 tx/s |
| _Per-snapshot TPS P95_ | 121.44 tx/s |
| _Per-snapshot TPS max_ | 186.29 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
