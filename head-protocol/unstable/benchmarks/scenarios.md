--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-25 22:14:07.28686241 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.2 | 140.83 | n/a | 212.2 | 212.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.3 | 106.18 | 96.00 | 9.3 | 23.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1035.99 | n/a | 28.1 | 28.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 120.62 | 122.63 | 8.2 | 21.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 993.88 | n/a | 29.5 | 29.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 168.46 | 163.59 | 5.9 | 8.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1177.05 | n/a | 49.3 | 50.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 135.46 | 141.54 | 14.6 | 22.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 874.95 | n/a | 66.7 | 67.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 96.06 | 97.65 | 20.4 | 28.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 970.76 | n/a | 60.0 | 60.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 102.67 | 97.70 | 19.2 | 32.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 796.98 | n/a | 110.3 | 111.9 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.0 | 92.99 | 90.33 | 31.9 | 42.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 653.49 | n/a | 131.2 | 135.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 78.41 | 77.34 | 37.7 | 49.6 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 687.10 | n/a | 127.8 | 130.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 2.1 | 42.04 | 43.86 | 71.1 | 267.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 212.2 |
| _P99_ | 212.8ms |
| _P95_ | 212.8ms |
| _P50_ | 212.4ms |
| _Tx validation time p50 (ms)_ | 196.9 |
| _End-to-end TPS_ | 140.83 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.39 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.3 |
| _P99_ | 87.1ms |
| _P95_ | 23.2ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 106.18 tx/s |
| _Sustained TPS_ | 96.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 106.18 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 28.7ms |
| _P95_ | 28.7ms |
| _P50_ | 28.4ms |
| _Tx validation time p50 (ms)_ | 11.8 |
| _End-to-end TPS_ | 1035.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.07 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 8.2 |
| _P99_ | 30.1ms |
| _P95_ | 21.6ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 120.62 tx/s |
| _Sustained TPS_ | 122.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 120.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 30.0ms |
| _P95_ | 29.9ms |
| _P50_ | 29.7ms |
| _Tx validation time p50 (ms)_ | 14.4 |
| _End-to-end TPS_ | 993.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 8.2ms |
| _P95_ | 8.0ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 168.46 tx/s |
| _Sustained TPS_ | 163.59 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 168.46 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 49.3 |
| _P99_ | 50.7ms |
| _P95_ | 50.7ms |
| _P50_ | 49.5ms |
| _Tx validation time p50 (ms)_ | 23.1 |
| _End-to-end TPS_ | 1177.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.24 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.6 |
| _P99_ | 24.5ms |
| _P95_ | 22.3ms |
| _P50_ | 12.7ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 135.46 tx/s |
| _Sustained TPS_ | 141.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 135.46 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.7 |
| _P99_ | 68.0ms |
| _P95_ | 67.5ms |
| _P50_ | 67.1ms |
| _Tx validation time p50 (ms)_ | 25.4 |
| _End-to-end TPS_ | 874.95 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.17 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 41.2ms |
| _P95_ | 28.0ms |
| _P50_ | 19.1ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 96.06 tx/s |
| _Sustained TPS_ | 97.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 60.0 |
| _P99_ | 60.7ms |
| _P95_ | 60.6ms |
| _P50_ | 60.2ms |
| _Tx validation time p50 (ms)_ | 28.3 |
| _End-to-end TPS_ | 970.76 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.36 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 35.2ms |
| _P95_ | 32.2ms |
| _P50_ | 17.6ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 102.67 tx/s |
| _Sustained TPS_ | 97.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 110.3 |
| _P99_ | 112.0ms |
| _P95_ | 111.9ms |
| _P50_ | 111.0ms |
| _Tx validation time p50 (ms)_ | 48.9 |
| _End-to-end TPS_ | 796.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.71 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.9 |
| _P99_ | 134.5ms |
| _P95_ | 42.9ms |
| _P50_ | 26.9ms |
| _Tx validation time p50 (ms)_ | 8.1 |
| _End-to-end TPS_ | 92.99 tx/s |
| _Sustained TPS_ | 90.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 63.03 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.2 |
| _P99_ | 135.9ms |
| _P95_ | 135.6ms |
| _P50_ | 131.4ms |
| _Tx validation time p50 (ms)_ | 50.1 |
| _End-to-end TPS_ | 653.49 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.52 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.7 |
| _P99_ | 52.9ms |
| _P95_ | 49.6ms |
| _P50_ | 37.2ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 78.41 tx/s |
| _Sustained TPS_ | 77.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 53.15 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 127.8 |
| _P99_ | 130.8ms |
| _P95_ | 130.7ms |
| _P50_ | 128.5ms |
| _Tx validation time p50 (ms)_ | 50.2 |
| _End-to-end TPS_ | 687.10 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.27 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 71.1 |
| _P99_ | 337.8ms |
| _P95_ | 267.4ms |
| _P50_ | 38.6ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 42.04 tx/s |
| _Sustained TPS_ | 43.86 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 28.49 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
