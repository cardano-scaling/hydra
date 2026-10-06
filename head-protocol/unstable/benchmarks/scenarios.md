--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-06 12:32:17.031678522 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.1 | 233.21 | n/a | 128.0 | 128.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.3 | 105.65 | 106.14 | 9.4 | 25.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 427.87 | n/a | 69.4 | 69.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 166.78 | 165.76 | 5.9 | 7.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 338.10 | n/a | 88.1 | 88.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 112.99 | 111.06 | 8.7 | 22.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 677.43 | n/a | 86.9 | 87.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 109.13 | 116.55 | 18.1 | 30.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 495.15 | n/a | 119.5 | 120.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.9 | 32.16 | 30.35 | 59.5 | 177.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 879.24 | n/a | 66.3 | 67.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.7 | 81.18 | 76.97 | 24.4 | 49.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.3 | 358.86 | n/a | 247.6 | 249.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.3 | 70.95 | 67.62 | 41.9 | 143.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 630.18 | n/a | 139.6 | 141.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 75.25 | 73.44 | 39.6 | 122.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 430.00 | n/a | 207.1 | 208.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.6 | 56.52 | 57.20 | 52.7 | 143.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.0 |
| _P99_ | 128.5ms |
| _P95_ | 128.5ms |
| _P50_ | 128.2ms |
| _Tx validation time p50 (ms)_ | 111.2 |
| _End-to-end TPS_ | 233.21 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.55 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.4 |
| _P99_ | 57.8ms |
| _P95_ | 25.8ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 2.0 |
| _End-to-end TPS_ | 105.65 tx/s |
| _Sustained TPS_ | 106.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 105.65 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 69.4 |
| _P99_ | 69.9ms |
| _P95_ | 69.9ms |
| _P50_ | 69.7ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 427.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 9.0ms |
| _P95_ | 7.6ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 166.78 tx/s |
| _Sustained TPS_ | 165.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 166.78 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 88.1 |
| _P99_ | 88.6ms |
| _P95_ | 88.5ms |
| _P50_ | 88.3ms |
| _Tx validation time p50 (ms)_ | 69.7 |
| _End-to-end TPS_ | 338.10 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.54 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 8.7 |
| _P99_ | 39.9ms |
| _P95_ | 22.4ms |
| _P50_ | 6.3ms |
| _Tx validation time p50 (ms)_ | 2.1 |
| _End-to-end TPS_ | 112.99 tx/s |
| _Sustained TPS_ | 111.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 112.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.9 |
| _P99_ | 87.8ms |
| _P95_ | 87.7ms |
| _P50_ | 87.2ms |
| _Tx validation time p50 (ms)_ | 48.4 |
| _End-to-end TPS_ | 677.43 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.58 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 54.5ms |
| _P95_ | 30.8ms |
| _P50_ | 15.2ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 109.13 tx/s |
| _Sustained TPS_ | 116.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 109.13 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 119.5 |
| _P99_ | 121.0ms |
| _P95_ | 120.9ms |
| _P50_ | 119.9ms |
| _Tx validation time p50 (ms)_ | 67.4 |
| _End-to-end TPS_ | 495.15 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.51 /s |
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
| _Avg. Confirmation Time (ms)_ | 59.5 |
| _P99_ | 222.4ms |
| _P95_ | 177.0ms |
| _P50_ | 34.0ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 32.16 tx/s |
| _Sustained TPS_ | 30.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 32.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.3 |
| _P99_ | 67.6ms |
| _P95_ | 67.2ms |
| _P50_ | 66.7ms |
| _Tx validation time p50 (ms)_ | 21.5 |
| _End-to-end TPS_ | 879.24 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.4 |
| _P99_ | 140.7ms |
| _P95_ | 49.1ms |
| _P50_ | 18.0ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 81.18 tx/s |
| _Sustained TPS_ | 76.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 81.18 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 247.6 |
| _P99_ | 249.6ms |
| _P95_ | 249.3ms |
| _P50_ | 248.0ms |
| _Tx validation time p50 (ms)_ | 152.2 |
| _End-to-end TPS_ | 358.86 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 7.97 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.9 |
| _P99_ | 174.5ms |
| _P95_ | 143.2ms |
| _P50_ | 25.3ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 70.95 tx/s |
| _Sustained TPS_ | 67.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 48.88 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 139.6 |
| _P99_ | 141.3ms |
| _P95_ | 141.2ms |
| _P50_ | 140.0ms |
| _Tx validation time p50 (ms)_ | 40.9 |
| _End-to-end TPS_ | 630.18 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.00 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 39.6 |
| _P99_ | 135.0ms |
| _P95_ | 122.2ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 75.25 tx/s |
| _Sustained TPS_ | 73.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 50.17 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 147.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 207.1 |
| _P99_ | 208.5ms |
| _P95_ | 208.5ms |
| _P50_ | 206.8ms |
| _Tx validation time p50 (ms)_ | 92.7 |
| _End-to-end TPS_ | 430.00 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.56 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 52.7 |
| _P99_ | 198.0ms |
| _P95_ | 143.6ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 56.52 tx/s |
| _Sustained TPS_ | 57.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 38.31 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
