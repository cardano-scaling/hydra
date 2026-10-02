--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-02 10:43:29.56498339 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1235.69 | n/a | 23.9 | 24.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 1.1 | 26.72 | 28.72 | 37.4 | 155.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 240.47 | n/a | 124.1 | 124.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.5 | 57.99 | 54.44 | 17.2 | 50.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.3 | 107.38 | n/a | 272.3 | 279.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 1.3 | 23.14 | 23.22 | 43.2 | 96.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1284.34 | n/a | 45.6 | 46.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 1.4 | 41.79 | 44.80 | 47.7 | 141.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.2 | 360.90 | n/a | 163.3 | 166.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.6 | 38.50 | 32.78 | 51.7 | 256.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1193.57 | n/a | 48.2 | 49.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 2.5 | 23.78 | 23.57 | 83.5 | 279.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.5 | 178.38 | n/a | 499.7 | 502.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 98.16 | 104.19 | 29.1 | 85.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 578.45 | n/a | 154.0 | 155.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 2.8 | 32.10 | 35.94 | 88.9 | 257.1 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 398.88 | n/a | 220.8 | 225.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 3.5 | 25.92 | 29.72 | 114.2 | 477.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 23.9 |
| _P99_ | 24.2ms |
| _P95_ | 24.2ms |
| _P50_ | 24.1ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 1235.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 82.38 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.4 |
| _P99_ | 204.1ms |
| _P95_ | 155.5ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 26.72 tx/s |
| _Sustained TPS_ | 28.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 26.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 124.1 |
| _P99_ | 124.5ms |
| _P95_ | 124.5ms |
| _P50_ | 124.3ms |
| _Tx validation time p50 (ms)_ | 99.1 |
| _End-to-end TPS_ | 240.47 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.03 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.2 |
| _P99_ | 143.7ms |
| _P95_ | 50.2ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 57.99 tx/s |
| _Sustained TPS_ | 54.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 57.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 272.3 |
| _P99_ | 279.2ms |
| _P95_ | 279.2ms |
| _P50_ | 279.0ms |
| _Tx validation time p50 (ms)_ | 67.5 |
| _End-to-end TPS_ | 107.38 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 7.16 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 43.2 |
| _P99_ | 113.3ms |
| _P95_ | 96.4ms |
| _P50_ | 41.5ms |
| _Tx validation time p50 (ms)_ | 2.6 |
| _End-to-end TPS_ | 23.14 tx/s |
| _Sustained TPS_ | 23.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 23.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 45.6 |
| _P99_ | 46.3ms |
| _P95_ | 46.1ms |
| _P50_ | 45.8ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 1284.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 42.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 47.7 |
| _P99_ | 152.2ms |
| _P95_ | 141.9ms |
| _P50_ | 14.2ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 41.79 tx/s |
| _Sustained TPS_ | 44.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 41.79 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 163.3 |
| _P99_ | 166.1ms |
| _P95_ | 166.0ms |
| _P50_ | 165.5ms |
| _Tx validation time p50 (ms)_ | 16.8 |
| _End-to-end TPS_ | 360.90 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.03 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 51.7 |
| _P99_ | 553.0ms |
| _P95_ | 256.3ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 4.1 |
| _End-to-end TPS_ | 38.50 tx/s |
| _Sustained TPS_ | 32.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 38.50 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 48.2 |
| _P99_ | 49.7ms |
| _P95_ | 49.2ms |
| _P50_ | 48.4ms |
| _Tx validation time p50 (ms)_ | 20.5 |
| _End-to-end TPS_ | 1193.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.79 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 83.5 |
| _P99_ | 340.6ms |
| _P95_ | 279.7ms |
| _P50_ | 56.6ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 23.78 tx/s |
| _Sustained TPS_ | 23.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 23.78 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 499.7 |
| _P99_ | 502.6ms |
| _P95_ | 502.5ms |
| _P50_ | 501.4ms |
| _Tx validation time p50 (ms)_ | 212.4 |
| _End-to-end TPS_ | 178.38 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.96 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.1 |
| _P99_ | 121.0ms |
| _P95_ | 85.1ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 98.16 tx/s |
| _Sustained TPS_ | 104.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 66.53 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.0 |
| _P99_ | 155.3ms |
| _P95_ | 155.3ms |
| _P50_ | 154.1ms |
| _Tx validation time p50 (ms)_ | 86.5 |
| _End-to-end TPS_ | 578.45 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.85 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 88.9 |
| _P99_ | 301.6ms |
| _P95_ | 257.1ms |
| _P50_ | 43.0ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 32.10 tx/s |
| _Sustained TPS_ | 35.94 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 21.76 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 220.8 |
| _P99_ | 225.1ms |
| _P95_ | 225.0ms |
| _P50_ | 221.9ms |
| _Tx validation time p50 (ms)_ | 28.2 |
| _End-to-end TPS_ | 398.88 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 8.86 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 114.2 |
| _P99_ | 605.4ms |
| _P95_ | 477.8ms |
| _P50_ | 50.4ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 25.92 tx/s |
| _Sustained TPS_ | 29.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 17.57 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      
