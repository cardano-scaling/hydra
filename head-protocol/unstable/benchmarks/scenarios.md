--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-19 18:10:17.320091687 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 969.89 | n/a | 30.0 | 30.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 212.21 | 214.49 | 4.7 | 6.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 966.48 | n/a | 30.4 | 30.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 177.17 | 175.97 | 5.6 | 7.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1131.56 | n/a | 25.6 | 26.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 183.56 | 177.90 | 5.4 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1033.40 | n/a | 56.5 | 57.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 147.11 | 146.02 | 13.4 | 18.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 788.50 | n/a | 74.1 | 75.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 100.54 | 100.55 | 19.7 | 26.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 874.74 | n/a | 67.0 | 68.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 101.48 | 98.88 | 19.5 | 26.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 675.84 | n/a | 128.2 | 130.9 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 112.04 | 112.40 | 26.3 | 33.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 624.50 | n/a | 140.3 | 142.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 79.39 | 77.79 | 37.5 | 45.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 680.99 | n/a | 129.3 | 131.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 87.79 | 85.15 | 33.7 | 42.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 30.0 |
| _P99_ | 30.7ms |
| _P95_ | 30.7ms |
| _P50_ | 30.1ms |
| _Tx validation time p50 (ms)_ | 23.3 |
| _End-to-end TPS_ | 969.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.66 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 7.2ms |
| _P95_ | 6.5ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 212.21 tx/s |
| _Sustained TPS_ | 214.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 212.21 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 30.4 |
| _P99_ | 30.9ms |
| _P95_ | 30.9ms |
| _P50_ | 30.6ms |
| _Tx validation time p50 (ms)_ | 13.2 |
| _End-to-end TPS_ | 966.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.43 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 8.1ms |
| _P95_ | 7.2ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 177.17 tx/s |
| _Sustained TPS_ | 175.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 25.6 |
| _P99_ | 26.3ms |
| _P95_ | 26.2ms |
| _P50_ | 25.7ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 1131.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 75.44 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 10.4ms |
| _P95_ | 6.7ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 183.56 tx/s |
| _Sustained TPS_ | 177.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 56.5 |
| _P99_ | 58.0ms |
| _P95_ | 57.9ms |
| _P50_ | 56.9ms |
| _Tx validation time p50 (ms)_ | 28.1 |
| _End-to-end TPS_ | 1033.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 34.45 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 13.4 |
| _P99_ | 21.0ms |
| _P95_ | 18.2ms |
| _P50_ | 12.8ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 147.11 tx/s |
| _Sustained TPS_ | 146.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 147.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 74.1 |
| _P99_ | 75.3ms |
| _P95_ | 75.2ms |
| _P50_ | 74.5ms |
| _Tx validation time p50 (ms)_ | 25.9 |
| _End-to-end TPS_ | 788.50 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.28 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.7 |
| _P99_ | 31.3ms |
| _P95_ | 26.6ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 100.54 tx/s |
| _Sustained TPS_ | 100.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 67.0 |
| _P99_ | 68.5ms |
| _P95_ | 68.4ms |
| _P50_ | 67.0ms |
| _Tx validation time p50 (ms)_ | 25.0 |
| _End-to-end TPS_ | 874.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.5 |
| _P99_ | 27.8ms |
| _P95_ | 26.4ms |
| _P50_ | 19.8ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 101.48 tx/s |
| _Sustained TPS_ | 98.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 128.2 |
| _P99_ | 131.1ms |
| _P95_ | 130.9ms |
| _P50_ | 129.9ms |
| _Tx validation time p50 (ms)_ | 39.1 |
| _End-to-end TPS_ | 675.84 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.02 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 26.3 |
| _P99_ | 34.5ms |
| _P95_ | 33.8ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 112.04 tx/s |
| _Sustained TPS_ | 112.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 75.94 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 140.3 |
| _P99_ | 142.6ms |
| _P95_ | 142.4ms |
| _P50_ | 141.6ms |
| _Tx validation time p50 (ms)_ | 40.2 |
| _End-to-end TPS_ | 624.50 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.88 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 37.5 |
| _P99_ | 49.2ms |
| _P95_ | 45.5ms |
| _P50_ | 38.2ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 79.39 tx/s |
| _Sustained TPS_ | 77.79 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 52.93 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 129.3 |
| _P99_ | 132.1ms |
| _P95_ | 131.9ms |
| _P50_ | 129.0ms |
| _Tx validation time p50 (ms)_ | 47.4 |
| _End-to-end TPS_ | 680.99 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.13 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.7 |
| _P99_ | 46.3ms |
| _P95_ | 42.5ms |
| _P50_ | 33.7ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 87.79 tx/s |
| _Sustained TPS_ | 85.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.50 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
