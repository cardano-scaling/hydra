--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-02 18:09:38.936797971 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1046.49 | n/a | 28.0 | 28.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 185.73 | 187.20 | 5.3 | 6.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 918.62 | n/a | 32.0 | 32.4 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 155.16 | 158.14 | 6.4 | 8.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1073.00 | n/a | 27.3 | 27.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 167.29 | 169.56 | 5.9 | 7.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 965.80 | n/a | 60.6 | 61.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 132.13 | 130.72 | 14.9 | 20.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 870.19 | n/a | 67.6 | 68.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 112.83 | 112.42 | 17.5 | 21.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 840.71 | n/a | 69.5 | 70.8 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 116.84 | 112.26 | 17.0 | 20.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 727.71 | n/a | 121.3 | 122.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 124.43 | 124.26 | 23.9 | 30.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 569.68 | n/a | 155.4 | 157.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 97.59 | 95.92 | 30.5 | 37.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 692.94 | n/a | 128.3 | 129.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 101.44 | 99.55 | 29.1 | 37.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.0 |
| _P99_ | 28.4ms |
| _P95_ | 28.4ms |
| _P50_ | 28.2ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 1046.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.77 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 7.0ms |
| _P95_ | 6.5ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 185.73 tx/s |
| _Sustained TPS_ | 187.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 185.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.0 |
| _P99_ | 32.5ms |
| _P95_ | 32.4ms |
| _P50_ | 32.3ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 918.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.24 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.4 |
| _P99_ | 9.0ms |
| _P95_ | 8.5ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 155.16 tx/s |
| _Sustained TPS_ | 158.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 155.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.3 |
| _P99_ | 27.7ms |
| _P95_ | 27.7ms |
| _P50_ | 27.5ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 1073.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.53 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 8.9ms |
| _P95_ | 7.6ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 167.29 tx/s |
| _Sustained TPS_ | 169.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 167.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 60.6 |
| _P99_ | 61.8ms |
| _P95_ | 61.8ms |
| _P50_ | 60.9ms |
| _Tx validation time p50 (ms)_ | 21.2 |
| _End-to-end TPS_ | 965.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.19 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 22.6ms |
| _P95_ | 20.2ms |
| _P50_ | 14.3ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 132.13 tx/s |
| _Sustained TPS_ | 130.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 132.13 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.6 |
| _P99_ | 68.7ms |
| _P95_ | 68.7ms |
| _P50_ | 67.8ms |
| _Tx validation time p50 (ms)_ | 20.7 |
| _End-to-end TPS_ | 870.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.01 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 23.5ms |
| _P95_ | 21.0ms |
| _P50_ | 17.4ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 112.83 tx/s |
| _Sustained TPS_ | 112.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 112.83 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 69.5 |
| _P99_ | 70.8ms |
| _P95_ | 70.8ms |
| _P50_ | 69.9ms |
| _Tx validation time p50 (ms)_ | 21.9 |
| _End-to-end TPS_ | 840.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.02 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.0 |
| _P99_ | 26.7ms |
| _P95_ | 20.6ms |
| _P50_ | 16.6ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 116.84 tx/s |
| _Sustained TPS_ | 112.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 116.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 121.3 |
| _P99_ | 122.8ms |
| _P95_ | 122.7ms |
| _P50_ | 122.3ms |
| _Tx validation time p50 (ms)_ | 39.5 |
| _End-to-end TPS_ | 727.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.17 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.9 |
| _P99_ | 30.7ms |
| _P95_ | 30.2ms |
| _P50_ | 24.1ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 124.43 tx/s |
| _Sustained TPS_ | 124.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 82.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 155.4 |
| _P99_ | 157.7ms |
| _P95_ | 157.6ms |
| _P50_ | 156.2ms |
| _Tx validation time p50 (ms)_ | 49.8 |
| _End-to-end TPS_ | 569.68 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.66 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 30.5 |
| _P99_ | 43.0ms |
| _P95_ | 37.2ms |
| _P50_ | 31.0ms |
| _Tx validation time p50 (ms)_ | 9.3 |
| _End-to-end TPS_ | 97.59 tx/s |
| _Sustained TPS_ | 95.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 66.15 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.3 |
| _P99_ | 129.4ms |
| _P95_ | 129.3ms |
| _P50_ | 128.8ms |
| _Tx validation time p50 (ms)_ | 41.3 |
| _End-to-end TPS_ | 692.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.40 /s |
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
| _Avg. Confirmation Time (ms)_ | 29.1 |
| _P99_ | 38.7ms |
| _P95_ | 37.8ms |
| _P50_ | 29.5ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 101.44 tx/s |
| _Sustained TPS_ | 99.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 68.76 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
