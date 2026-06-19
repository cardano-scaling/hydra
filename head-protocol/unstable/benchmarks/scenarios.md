--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-19 14:11:27.658637955 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.0 | 654.87 | 3140.16 | 45.2 | 45.5 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 185.96 | 218.77 | 5.3 | 10.2 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 264.57 | 1350.47 | 112.6 | 113.1 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 143.13 | 144.69 | 6.9 | 9.5 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.3 | 117.72 | 2799.56 | 254.3 | 254.7 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.6 | 51.66 | 160.99 | 19.3 | 109.7 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 353.58 | 3353.75 | 167.8 | 168.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 98.20 | 150.56 | 20.2 | 56.4 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.4 | 156.53 | 947.82 | 381.7 | 383.0 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.1 | 53.77 | 78.36 | 36.7 | 83.4 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 361.66 | 930.18 | 164.0 | 164.8 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 1.2 | 49.92 | 93.99 | 39.8 | 113.9 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.2 | 396.53 | 2082.80 | 224.1 | 225.7 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 0.9 | 99.35 | 84.68 | 29.5 | 52.2 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.5 | 190.95 | 708.45 | 466.9 | 469.8 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.3 | 39.03 | 53.06 | 75.0 | 167.9 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 356.96 | 1737.30 | 248.8 | 249.9 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.2 | 73.79 | 69.73 | 39.7 | 56.2 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 45.2 |
| _P99_ | 45.6ms |
| _P95_ | 45.5ms |
| _P50_ | 45.3ms |
| _End-to-end TPS_ | 654.87 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 3140.16 tx/s |
| _Per-snapshot TPS P95_ | 5944.45 tx/s |
| _Per-snapshot TPS max_ | 6193.71 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 13.4ms |
| _P95_ | 10.2ms |
| _P50_ | 4.5ms |
| _End-to-end TPS_ | 185.96 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 218.77 tx/s |
| _Per-snapshot TPS P95_ | 239.24 tx/s |
| _Per-snapshot TPS max_ | 239.54 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 112.6 |
| _P99_ | 113.2ms |
| _P95_ | 113.1ms |
| _P50_ | 112.9ms |
| _End-to-end TPS_ | 264.57 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1350.47 tx/s |
| _Per-snapshot TPS P95_ | 2557.12 tx/s |
| _Per-snapshot TPS max_ | 2664.38 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.9 |
| _P99_ | 10.1ms |
| _P95_ | 9.5ms |
| _P50_ | 6.8ms |
| _End-to-end TPS_ | 143.13 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 144.69 tx/s |
| _Per-snapshot TPS P95_ | 196.45 tx/s |
| _Per-snapshot TPS max_ | 199.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 254.3 |
| _P99_ | 254.7ms |
| _P95_ | 254.7ms |
| _P50_ | 254.5ms |
| _End-to-end TPS_ | 117.72 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2799.56 tx/s |
| _Per-snapshot TPS P95_ | 5315.56 tx/s |
| _Per-snapshot TPS max_ | 5539.20 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 124.6ms |
| _P95_ | 109.7ms |
| _P50_ | 6.1ms |
| _End-to-end TPS_ | 51.66 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 160.99 tx/s |
| _Per-snapshot TPS P95_ | 205.86 tx/s |
| _Per-snapshot TPS max_ | 207.94 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 167.8 |
| _P99_ | 169.0ms |
| _P95_ | 168.9ms |
| _P50_ | 168.0ms |
| _End-to-end TPS_ | 353.58 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 3353.75 tx/s |
| _Per-snapshot TPS P95_ | 6366.51 tx/s |
| _Per-snapshot TPS max_ | 6634.31 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 20.2 |
| _P99_ | 70.9ms |
| _P95_ | 56.4ms |
| _P50_ | 14.3ms |
| _End-to-end TPS_ | 98.20 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 150.56 tx/s |
| _Per-snapshot TPS P95_ | 179.19 tx/s |
| _Per-snapshot TPS max_ | 189.06 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 381.7 |
| _P99_ | 383.0ms |
| _P95_ | 383.0ms |
| _P50_ | 382.2ms |
| _End-to-end TPS_ | 156.53 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 947.82 tx/s |
| _Per-snapshot TPS P95_ | 1798.30 tx/s |
| _Per-snapshot TPS max_ | 1873.90 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 36.7 |
| _P99_ | 127.0ms |
| _P95_ | 83.4ms |
| _P50_ | 29.4ms |
| _End-to-end TPS_ | 53.77 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 78.36 tx/s |
| _Per-snapshot TPS P95_ | 121.50 tx/s |
| _Per-snapshot TPS max_ | 134.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 164.0 |
| _P99_ | 165.3ms |
| _P95_ | 164.8ms |
| _P50_ | 164.5ms |
| _End-to-end TPS_ | 361.66 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 930.18 tx/s |
| _Per-snapshot TPS P95_ | 1760.58 tx/s |
| _Per-snapshot TPS max_ | 1834.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 39.8 |
| _P99_ | 117.3ms |
| _P95_ | 113.9ms |
| _P50_ | 23.3ms |
| _End-to-end TPS_ | 49.92 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 93.99 tx/s |
| _Per-snapshot TPS P95_ | 139.20 tx/s |
| _Per-snapshot TPS max_ | 181.06 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 224.1 |
| _P99_ | 225.8ms |
| _P95_ | 225.7ms |
| _P50_ | 225.2ms |
| _End-to-end TPS_ | 396.53 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 2082.80 tx/s |
| _Per-snapshot TPS P95_ | 3952.89 tx/s |
| _Per-snapshot TPS max_ | 4119.12 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 65.5ms |
| _P95_ | 52.2ms |
| _P50_ | 26.6ms |
| _End-to-end TPS_ | 99.35 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 84.68 tx/s |
| _Per-snapshot TPS P95_ | 214.69 tx/s |
| _Per-snapshot TPS max_ | 226.39 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 466.9 |
| _P99_ | 469.9ms |
| _P95_ | 469.8ms |
| _P50_ | 468.8ms |
| _End-to-end TPS_ | 190.95 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 708.45 tx/s |
| _Per-snapshot TPS P95_ | 1343.83 tx/s |
| _Per-snapshot TPS max_ | 1400.31 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 75.0 |
| _P99_ | 201.5ms |
| _P95_ | 167.9ms |
| _P50_ | 71.4ms |
| _End-to-end TPS_ | 39.03 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 53.06 tx/s |
| _Per-snapshot TPS P95_ | 140.58 tx/s |
| _Per-snapshot TPS max_ | 191.86 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 248.8 |
| _P99_ | 250.2ms |
| _P95_ | 249.9ms |
| _P50_ | 249.3ms |
| _End-to-end TPS_ | 356.96 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1737.30 tx/s |
| _Per-snapshot TPS P95_ | 3296.85 tx/s |
| _Per-snapshot TPS max_ | 3435.48 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 39.7 |
| _P99_ | 68.8ms |
| _P95_ | 56.2ms |
| _P50_ | 38.7ms |
| _End-to-end TPS_ | 73.79 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 69.73 tx/s |
| _Per-snapshot TPS P95_ | 150.95 tx/s |
| _Per-snapshot TPS max_ | 190.32 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
