--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-12 14:54:11.011670018 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 518.19 | 1748.98 | 57.0 | 57.6 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 174.44 | 183.56 | 5.7 | 7.8 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 387.54 | 901.65 | 76.2 | 77.2 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 116.12 | 117.73 | 8.5 | 11.9 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 452.11 | 1587.74 | 65.5 | 66.1 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 135.86 | 139.31 | 7.3 | 9.5 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 361.18 | 1510.03 | 164.1 | 165.1 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 109.88 | 117.73 | 17.9 | 24.6 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 292.82 | 709.49 | 202.2 | 204.5 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 61.46 | 62.68 | 31.8 | 44.3 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 334.85 | 972.95 | 177.1 | 178.0 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 78.30 | 83.35 | 25.3 | 33.9 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 305.02 | 955.30 | 290.5 | 293.3 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 92.80 | 76.06 | 31.9 | 41.6 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 241.91 | 387.60 | 365.9 | 368.6 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.29 | 41.61 | 66.5 | 103.1 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 287.02 | 711.67 | 308.6 | 312.1 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 62.03 | 58.71 | 47.8 | 65.9 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 57.0 |
| _P99_ | 57.7ms |
| _P95_ | 57.6ms |
| _P50_ | 57.3ms |
| _End-to-end TPS_ | 518.19 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1748.98 tx/s |
| _Per-snapshot TPS P95_ | 3304.89 tx/s |
| _Per-snapshot TPS max_ | 3443.20 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 9.3ms |
| _P95_ | 7.8ms |
| _P50_ | 5.4ms |
| _End-to-end TPS_ | 174.44 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 183.56 tx/s |
| _Per-snapshot TPS P95_ | 196.68 tx/s |
| _Per-snapshot TPS max_ | 201.43 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 76.2 |
| _P99_ | 77.2ms |
| _P95_ | 77.2ms |
| _P50_ | 76.6ms |
| _End-to-end TPS_ | 387.54 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 901.65 tx/s |
| _Per-snapshot TPS P95_ | 1698.41 tx/s |
| _Per-snapshot TPS max_ | 1769.23 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.5 |
| _P99_ | 13.4ms |
| _P95_ | 11.9ms |
| _P50_ | 8.3ms |
| _End-to-end TPS_ | 116.12 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 117.73 tx/s |
| _Per-snapshot TPS P95_ | 165.82 tx/s |
| _Per-snapshot TPS max_ | 173.78 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.5 |
| _P99_ | 66.1ms |
| _P95_ | 66.1ms |
| _P50_ | 65.7ms |
| _End-to-end TPS_ | 452.11 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1587.74 tx/s |
| _Per-snapshot TPS P95_ | 3000.95 tx/s |
| _Per-snapshot TPS max_ | 3126.57 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.3 |
| _P99_ | 10.6ms |
| _P95_ | 9.5ms |
| _P50_ | 7.1ms |
| _End-to-end TPS_ | 135.86 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 139.31 tx/s |
| _Per-snapshot TPS P95_ | 169.88 tx/s |
| _Per-snapshot TPS max_ | 184.77 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 164.1 |
| _P99_ | 165.6ms |
| _P95_ | 165.1ms |
| _P50_ | 164.5ms |
| _End-to-end TPS_ | 361.18 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1510.03 tx/s |
| _Per-snapshot TPS P95_ | 2862.89 tx/s |
| _Per-snapshot TPS max_ | 2983.15 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 17.9 |
| _P99_ | 26.5ms |
| _P95_ | 24.6ms |
| _P50_ | 17.0ms |
| _End-to-end TPS_ | 109.88 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 117.73 tx/s |
| _Per-snapshot TPS P95_ | 141.67 tx/s |
| _Per-snapshot TPS max_ | 146.94 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 202.2 |
| _P99_ | 204.6ms |
| _P95_ | 204.5ms |
| _P50_ | 202.8ms |
| _End-to-end TPS_ | 292.82 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 709.49 tx/s |
| _Per-snapshot TPS P95_ | 1342.48 tx/s |
| _Per-snapshot TPS max_ | 1398.75 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 31.8 |
| _P99_ | 47.6ms |
| _P95_ | 44.3ms |
| _P50_ | 31.1ms |
| _End-to-end TPS_ | 61.46 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 62.68 tx/s |
| _Per-snapshot TPS P95_ | 97.87 tx/s |
| _Per-snapshot TPS max_ | 117.84 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 177.1 |
| _P99_ | 178.2ms |
| _P95_ | 178.0ms |
| _P50_ | 177.6ms |
| _End-to-end TPS_ | 334.85 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 972.95 tx/s |
| _Per-snapshot TPS P95_ | 1842.52 tx/s |
| _Per-snapshot TPS max_ | 1919.82 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 25.3 |
| _P99_ | 39.5ms |
| _P95_ | 33.9ms |
| _P50_ | 24.8ms |
| _End-to-end TPS_ | 78.30 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 83.35 tx/s |
| _Per-snapshot TPS P95_ | 118.86 tx/s |
| _Per-snapshot TPS max_ | 136.45 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 290.5 |
| _P99_ | 293.4ms |
| _P95_ | 293.3ms |
| _P50_ | 292.1ms |
| _End-to-end TPS_ | 305.02 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 955.30 tx/s |
| _Per-snapshot TPS P95_ | 1811.41 tx/s |
| _Per-snapshot TPS max_ | 1887.50 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 31.9 |
| _P99_ | 44.9ms |
| _P95_ | 41.6ms |
| _P50_ | 30.9ms |
| _End-to-end TPS_ | 92.80 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 76.06 tx/s |
| _Per-snapshot TPS P95_ | 172.92 tx/s |
| _Per-snapshot TPS max_ | 193.46 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 365.9 |
| _P99_ | 369.1ms |
| _P95_ | 368.6ms |
| _P50_ | 368.0ms |
| _End-to-end TPS_ | 241.91 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 387.60 tx/s |
| _Per-snapshot TPS P95_ | 732.88 tx/s |
| _Per-snapshot TPS max_ | 763.57 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 66.5 |
| _P99_ | 121.6ms |
| _P95_ | 103.1ms |
| _P50_ | 64.4ms |
| _End-to-end TPS_ | 43.29 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 41.61 tx/s |
| _Per-snapshot TPS P95_ | 90.79 tx/s |
| _Per-snapshot TPS max_ | 141.42 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 308.6 |
| _P99_ | 312.4ms |
| _P95_ | 312.1ms |
| _P50_ | 308.7ms |
| _End-to-end TPS_ | 287.02 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 711.67 tx/s |
| _Per-snapshot TPS P95_ | 1348.53 tx/s |
| _Per-snapshot TPS max_ | 1405.14 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 47.8 |
| _P99_ | 83.8ms |
| _P95_ | 65.9ms |
| _P50_ | 46.1ms |
| _End-to-end TPS_ | 62.03 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 58.71 tx/s |
| _Per-snapshot TPS P95_ | 126.94 tx/s |
| _Per-snapshot TPS max_ | 140.41 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
