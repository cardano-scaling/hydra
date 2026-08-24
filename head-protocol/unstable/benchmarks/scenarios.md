--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-24 11:34:58.345045578 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 943.46 | n/a | 30.9 | 31.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 202.53 | 199.91 | 4.9 | 6.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1024.29 | n/a | 28.5 | 29.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 161.72 | 160.02 | 6.1 | 7.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 820.89 | n/a | 35.3 | 36.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 170.99 | 171.34 | 5.8 | 10.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 929.37 | n/a | 63.0 | 63.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 134.38 | 138.74 | 14.7 | 19.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 730.13 | n/a | 79.9 | 81.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 97.58 | 96.46 | 20.2 | 26.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 884.00 | n/a | 65.5 | 67.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.31 | 97.14 | 19.9 | 27.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 721.10 | n/a | 119.9 | 124.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 103.21 | 104.37 | 28.3 | 37.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 494.78 | n/a | 177.8 | 181.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.3 | 69.47 | 69.36 | 42.8 | 49.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 654.59 | n/a | 134.1 | 137.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 87.80 | 85.44 | 33.9 | 43.2 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 30.9 |
| _P99_ | 31.5ms |
| _P95_ | 31.4ms |
| _P50_ | 31.0ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 943.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.90 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 9.7ms |
| _P95_ | 6.6ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 202.53 tx/s |
| _Sustained TPS_ | 199.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 202.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 28.5 |
| _P99_ | 29.1ms |
| _P95_ | 29.0ms |
| _P50_ | 28.7ms |
| _Tx validation time p50 (ms)_ | 11.7 |
| _End-to-end TPS_ | 1024.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 68.29 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 11.1ms |
| _P95_ | 7.4ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 161.72 tx/s |
| _Sustained TPS_ | 160.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 161.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 35.3 |
| _P99_ | 36.3ms |
| _P95_ | 36.3ms |
| _P50_ | 35.8ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 820.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 11.9ms |
| _P95_ | 10.0ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 170.99 tx/s |
| _Sustained TPS_ | 171.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 170.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 63.0 |
| _P99_ | 63.8ms |
| _P95_ | 63.6ms |
| _P50_ | 63.1ms |
| _Tx validation time p50 (ms)_ | 22.9 |
| _End-to-end TPS_ | 929.37 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.98 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 14.7 |
| _P99_ | 20.9ms |
| _P95_ | 19.4ms |
| _P50_ | 14.0ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 134.38 tx/s |
| _Sustained TPS_ | 138.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 134.38 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 79.9 |
| _P99_ | 81.9ms |
| _P95_ | 81.9ms |
| _P50_ | 80.4ms |
| _Tx validation time p50 (ms)_ | 29.1 |
| _End-to-end TPS_ | 730.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.34 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 20.2 |
| _P99_ | 30.6ms |
| _P95_ | 26.6ms |
| _P50_ | 19.6ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 97.58 tx/s |
| _Sustained TPS_ | 96.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 65.5 |
| _P99_ | 67.4ms |
| _P95_ | 67.0ms |
| _P50_ | 65.7ms |
| _Tx validation time p50 (ms)_ | 23.7 |
| _End-to-end TPS_ | 884.00 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.9 |
| _P99_ | 31.1ms |
| _P95_ | 27.2ms |
| _P50_ | 19.6ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 99.31 tx/s |
| _Sustained TPS_ | 97.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.31 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 119.9 |
| _P99_ | 124.5ms |
| _P95_ | 124.4ms |
| _P50_ | 120.8ms |
| _Tx validation time p50 (ms)_ | 49.1 |
| _End-to-end TPS_ | 721.10 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.02 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 28.3 |
| _P99_ | 39.6ms |
| _P95_ | 37.7ms |
| _P50_ | 27.0ms |
| _Tx validation time p50 (ms)_ | 8.1 |
| _End-to-end TPS_ | 103.21 tx/s |
| _Sustained TPS_ | 104.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 69.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 177.8 |
| _P99_ | 181.5ms |
| _P95_ | 181.4ms |
| _P50_ | 180.1ms |
| _Tx validation time p50 (ms)_ | 40.4 |
| _End-to-end TPS_ | 494.78 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.00 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 42.8 |
| _P99_ | 154.5ms |
| _P95_ | 49.2ms |
| _P50_ | 38.0ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 69.47 tx/s |
| _Sustained TPS_ | 69.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 46.31 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 134.1 |
| _P99_ | 137.3ms |
| _P95_ | 137.2ms |
| _P50_ | 134.1ms |
| _Tx validation time p50 (ms)_ | 46.9 |
| _End-to-end TPS_ | 654.59 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.55 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 49.0ms |
| _P95_ | 43.2ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 9.3 |
| _End-to-end TPS_ | 87.80 tx/s |
| _Sustained TPS_ | 85.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 58.53 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
