--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-19 13:14:43.846160913 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.1 | 200.14 | n/a | 144.9 | 149.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 238.63 | 256.31 | 4.1 | 7.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 779.64 | n/a | 37.3 | 38.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.4 | 85.36 | 167.38 | 11.7 | 15.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 318.86 | n/a | 93.5 | 93.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 229.19 | 223.97 | 4.3 | 5.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1283.13 | n/a | 45.6 | 46.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.6 | 101.27 | 141.43 | 19.2 | 63.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 506.06 | n/a | 116.5 | 117.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 120.73 | 118.06 | 16.4 | 27.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1161.66 | n/a | 50.3 | 50.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 132.58 | 128.01 | 14.9 | 19.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 987.15 | n/a | 88.7 | 89.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.7 | 51.57 | 57.85 | 57.9 | 190.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 721.64 | n/a | 121.0 | 123.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 95.54 | 99.20 | 31.1 | 45.1 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 595.67 | n/a | 149.4 | 150.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.4 | 64.34 | 57.55 | 46.5 | 195.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 144.9 |
| _P99_ | 149.7ms |
| _P95_ | 149.7ms |
| _P50_ | 149.4ms |
| _Tx validation time p50 (ms)_ | 8.6 |
| _End-to-end TPS_ | 200.14 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.34 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.1 |
| _P99_ | 11.9ms |
| _P95_ | 7.8ms |
| _P50_ | 3.4ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 238.63 tx/s |
| _Sustained TPS_ | 256.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 238.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 37.3 |
| _P99_ | 38.3ms |
| _P95_ | 38.2ms |
| _P50_ | 38.0ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 779.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 51.98 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 11.7 |
| _P99_ | 135.7ms |
| _P95_ | 15.6ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 85.36 tx/s |
| _Sustained TPS_ | 167.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 85.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 93.5 |
| _P99_ | 93.9ms |
| _P95_ | 93.8ms |
| _P50_ | 93.6ms |
| _Tx validation time p50 (ms)_ | 87.2 |
| _End-to-end TPS_ | 318.86 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 7.9ms |
| _P95_ | 5.7ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 229.19 tx/s |
| _Sustained TPS_ | 223.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 229.19 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 45.6 |
| _P99_ | 46.2ms |
| _P95_ | 46.1ms |
| _P50_ | 45.8ms |
| _Tx validation time p50 (ms)_ | 15.2 |
| _End-to-end TPS_ | 1283.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 42.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 126.5ms |
| _P95_ | 63.6ms |
| _P50_ | 10.3ms |
| _Tx validation time p50 (ms)_ | 2.8 |
| _End-to-end TPS_ | 101.27 tx/s |
| _Sustained TPS_ | 141.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.27 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 116.5 |
| _P99_ | 118.0ms |
| _P95_ | 117.9ms |
| _P50_ | 116.9ms |
| _Tx validation time p50 (ms)_ | 78.8 |
| _End-to-end TPS_ | 506.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.87 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 16.4 |
| _P99_ | 28.1ms |
| _P95_ | 27.4ms |
| _P50_ | 15.2ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 120.73 tx/s |
| _Sustained TPS_ | 118.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 120.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 147.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 50.3 |
| _P99_ | 51.2ms |
| _P95_ | 50.9ms |
| _P50_ | 50.4ms |
| _Tx validation time p50 (ms)_ | 16.2 |
| _End-to-end TPS_ | 1161.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 38.72 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 20.3ms |
| _P95_ | 19.8ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 132.58 tx/s |
| _Sustained TPS_ | 128.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 132.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 88.7 |
| _P99_ | 90.0ms |
| _P95_ | 89.8ms |
| _P50_ | 89.4ms |
| _Tx validation time p50 (ms)_ | 32.3 |
| _End-to-end TPS_ | 987.15 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.94 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 57.9 |
| _P99_ | 246.7ms |
| _P95_ | 190.4ms |
| _P50_ | 21.0ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 51.57 tx/s |
| _Sustained TPS_ | 57.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 34.38 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 121.0 |
| _P99_ | 123.5ms |
| _P95_ | 123.4ms |
| _P50_ | 122.7ms |
| _Tx validation time p50 (ms)_ | 59.5 |
| _End-to-end TPS_ | 721.64 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.04 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 55.7ms |
| _P95_ | 45.1ms |
| _P50_ | 29.5ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 95.54 tx/s |
| _Sustained TPS_ | 99.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 63.69 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 149.4 |
| _P99_ | 150.4ms |
| _P95_ | 150.3ms |
| _P50_ | 150.0ms |
| _Tx validation time p50 (ms)_ | 87.9 |
| _End-to-end TPS_ | 595.67 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.24 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 46.5 |
| _P99_ | 216.7ms |
| _P95_ | 195.1ms |
| _P50_ | 27.4ms |
| _Tx validation time p50 (ms)_ | 7.6 |
| _End-to-end TPS_ | 64.34 tx/s |
| _Sustained TPS_ | 57.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 42.90 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
