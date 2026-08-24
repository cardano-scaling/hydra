--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-24 16:42:56.698447987 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1211.80 | n/a | 24.1 | 24.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 217.73 | 217.12 | 4.5 | 6.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1014.56 | n/a | 28.8 | 29.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 171.08 | 170.82 | 5.8 | 7.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1112.73 | n/a | 26.3 | 26.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 191.15 | 188.11 | 5.2 | 6.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 915.30 | n/a | 63.2 | 64.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 132.25 | 133.65 | 14.9 | 18.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 734.62 | n/a | 79.8 | 81.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 87.40 | 88.25 | 22.5 | 29.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 831.57 | n/a | 70.4 | 71.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 101.54 | 97.56 | 19.5 | 25.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 804.97 | n/a | 106.0 | 109.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 109.63 | 110.03 | 27.1 | 34.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 573.52 | n/a | 151.2 | 156.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 78.59 | 77.71 | 37.7 | 49.6 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 742.23 | n/a | 118.8 | 121.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.10 | 84.34 | 33.9 | 45.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.1 |
| _P99_ | 24.5ms |
| _P95_ | 24.5ms |
| _P50_ | 24.2ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 1211.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 80.79 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 6.9ms |
| _P95_ | 6.2ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 217.73 tx/s |
| _Sustained TPS_ | 217.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 217.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.8 |
| _P99_ | 29.4ms |
| _P95_ | 29.3ms |
| _P50_ | 29.0ms |
| _Tx validation time p50 (ms)_ | 14.3 |
| _End-to-end TPS_ | 1014.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 67.64 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 8.1ms |
| _P95_ | 7.2ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 171.08 tx/s |
| _Sustained TPS_ | 170.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.08 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.3 |
| _P99_ | 26.8ms |
| _P95_ | 26.7ms |
| _P50_ | 26.5ms |
| _Tx validation time p50 (ms)_ | 16.7 |
| _End-to-end TPS_ | 1112.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 74.18 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 6.6ms |
| _P95_ | 6.4ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 191.15 tx/s |
| _Sustained TPS_ | 188.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 191.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.2 |
| _P99_ | 65.0ms |
| _P95_ | 64.6ms |
| _P50_ | 63.5ms |
| _Tx validation time p50 (ms)_ | 20.7 |
| _End-to-end TPS_ | 915.30 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.51 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 19.5ms |
| _P95_ | 18.5ms |
| _P50_ | 14.7ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 132.25 tx/s |
| _Sustained TPS_ | 133.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 132.25 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.8 |
| _P99_ | 81.6ms |
| _P95_ | 81.5ms |
| _P50_ | 79.8ms |
| _Tx validation time p50 (ms)_ | 27.2 |
| _End-to-end TPS_ | 734.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.49 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.5 |
| _P99_ | 31.6ms |
| _P95_ | 29.3ms |
| _P50_ | 22.1ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 87.40 tx/s |
| _Sustained TPS_ | 88.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 87.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.4 |
| _P99_ | 71.6ms |
| _P95_ | 71.1ms |
| _P50_ | 70.6ms |
| _Tx validation time p50 (ms)_ | 27.6 |
| _End-to-end TPS_ | 831.57 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.72 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.5 |
| _P99_ | 29.7ms |
| _P95_ | 25.1ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 101.54 tx/s |
| _Sustained TPS_ | 97.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 106.0 |
| _P99_ | 110.2ms |
| _P95_ | 109.8ms |
| _P50_ | 107.8ms |
| _Tx validation time p50 (ms)_ | 52.8 |
| _End-to-end TPS_ | 804.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.89 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.1 |
| _P99_ | 36.9ms |
| _P95_ | 34.0ms |
| _P50_ | 27.3ms |
| _Tx validation time p50 (ms)_ | 7.4 |
| _End-to-end TPS_ | 109.63 tx/s |
| _Sustained TPS_ | 110.03 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 73.09 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 151.2 |
| _P99_ | 156.7ms |
| _P95_ | 156.6ms |
| _P50_ | 154.4ms |
| _Tx validation time p50 (ms)_ | 57.0 |
| _End-to-end TPS_ | 573.52 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.74 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.7 |
| _P99_ | 52.8ms |
| _P95_ | 49.6ms |
| _P50_ | 37.3ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 78.59 tx/s |
| _Sustained TPS_ | 77.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 53.26 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 118.8 |
| _P99_ | 121.0ms |
| _P95_ | 121.0ms |
| _P50_ | 119.5ms |
| _Tx validation time p50 (ms)_ | 65.4 |
| _End-to-end TPS_ | 742.23 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.49 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 49.6ms |
| _P95_ | 45.3ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 88.10 tx/s |
| _Sustained TPS_ | 84.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.71 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
