--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-26 11:35:39.241254974 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 967.21 | n/a | 30.1 | 30.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 180.07 | 176.74 | 5.5 | 6.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 713.11 | n/a | 41.2 | 41.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 164.05 | 164.13 | 6.0 | 6.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 859.91 | n/a | 33.8 | 34.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 171.09 | 171.81 | 5.8 | 7.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 822.46 | n/a | 71.1 | 72.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 125.89 | 126.27 | 15.7 | 19.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 715.89 | n/a | 81.9 | 82.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 95.28 | 92.87 | 20.8 | 25.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 700.08 | n/a | 84.3 | 85.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.92 | 96.62 | 19.8 | 26.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 673.97 | n/a | 130.5 | 132.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.55 | 103.61 | 28.2 | 34.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 642.89 | n/a | 136.4 | 138.9 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 82.71 | 83.88 | 35.6 | 45.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 622.51 | n/a | 141.5 | 144.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.29 | 83.53 | 34.9 | 43.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.1 |
| _P99_ | 30.9ms |
| _P95_ | 30.8ms |
| _P50_ | 30.3ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 967.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.48 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 14.7ms |
| _P95_ | 6.6ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 180.07 tx/s |
| _Sustained TPS_ | 176.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 180.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 41.2 |
| _P99_ | 41.9ms |
| _P95_ | 41.9ms |
| _P50_ | 41.5ms |
| _Tx validation time p50 (ms)_ | 18.4 |
| _End-to-end TPS_ | 713.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 47.54 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 7.4ms |
| _P95_ | 6.6ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 164.05 tx/s |
| _Sustained TPS_ | 164.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.05 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.8 |
| _P99_ | 34.6ms |
| _P95_ | 34.6ms |
| _P50_ | 34.0ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 859.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 8.0ms |
| _P95_ | 7.8ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 171.09 tx/s |
| _Sustained TPS_ | 171.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.09 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 71.1 |
| _P99_ | 72.8ms |
| _P95_ | 72.7ms |
| _P50_ | 71.5ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 822.46 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.42 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.7 |
| _P99_ | 25.0ms |
| _P95_ | 19.5ms |
| _P50_ | 15.4ms |
| _Tx validation time p50 (ms)_ | 4.1 |
| _End-to-end TPS_ | 125.89 tx/s |
| _Sustained TPS_ | 126.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 125.89 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.9 |
| _P99_ | 83.0ms |
| _P95_ | 82.9ms |
| _P50_ | 82.2ms |
| _Tx validation time p50 (ms)_ | 32.0 |
| _End-to-end TPS_ | 715.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.86 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.8 |
| _P99_ | 32.2ms |
| _P95_ | 25.8ms |
| _P50_ | 20.4ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 95.28 tx/s |
| _Sustained TPS_ | 92.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.28 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.3 |
| _P99_ | 85.6ms |
| _P95_ | 85.5ms |
| _P50_ | 84.8ms |
| _Tx validation time p50 (ms)_ | 27.6 |
| _End-to-end TPS_ | 700.08 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.34 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.8 |
| _P99_ | 26.9ms |
| _P95_ | 26.3ms |
| _P50_ | 18.6ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 99.92 tx/s |
| _Sustained TPS_ | 96.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 130.5 |
| _P99_ | 132.9ms |
| _P95_ | 132.8ms |
| _P50_ | 132.3ms |
| _Tx validation time p50 (ms)_ | 63.6 |
| _End-to-end TPS_ | 673.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.98 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.2 |
| _P99_ | 36.5ms |
| _P95_ | 34.4ms |
| _P50_ | 28.1ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 104.55 tx/s |
| _Sustained TPS_ | 103.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 72.02 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 136.4 |
| _P99_ | 139.1ms |
| _P95_ | 138.9ms |
| _P50_ | 136.5ms |
| _Tx validation time p50 (ms)_ | 74.0 |
| _End-to-end TPS_ | 642.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.29 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 50.4ms |
| _P95_ | 45.9ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 82.71 tx/s |
| _Sustained TPS_ | 83.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.06 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 141.5 |
| _P99_ | 144.4ms |
| _P95_ | 144.3ms |
| _P50_ | 141.8ms |
| _Tx validation time p50 (ms)_ | 57.2 |
| _End-to-end TPS_ | 622.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.83 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 45.5ms |
| _P95_ | 43.0ms |
| _P50_ | 35.5ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 85.29 tx/s |
| _Sustained TPS_ | 83.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.86 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
