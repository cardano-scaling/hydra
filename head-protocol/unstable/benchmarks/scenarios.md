--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-01 10:50:58.137851039 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 836.10 | n/a | 35.2 | 35.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 184.15 | 189.85 | 5.4 | 8.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 850.04 | n/a | 34.3 | 35.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 166.94 | 171.36 | 5.9 | 7.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 981.18 | n/a | 29.9 | 30.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 177.87 | 175.06 | 5.6 | 7.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 939.38 | n/a | 62.3 | 63.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 139.86 | 138.65 | 14.1 | 18.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 815.64 | n/a | 72.1 | 73.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 107.67 | 106.15 | 18.4 | 23.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 929.73 | n/a | 62.9 | 64.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 107.77 | 102.73 | 18.4 | 24.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 706.43 | n/a | 124.1 | 125.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 113.57 | 114.15 | 25.9 | 33.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 646.74 | n/a | 135.2 | 138.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 86.54 | 84.62 | 33.9 | 44.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 672.62 | n/a | 130.5 | 132.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 93.77 | 90.21 | 31.6 | 39.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 35.7ms |
| _P95_ | 35.6ms |
| _P50_ | 35.4ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 836.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.74 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 9.8ms |
| _P95_ | 8.2ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 184.15 tx/s |
| _Sustained TPS_ | 189.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 184.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.3 |
| _P99_ | 35.1ms |
| _P95_ | 35.0ms |
| _P50_ | 34.5ms |
| _Tx validation time p50 (ms)_ | 17.3 |
| _End-to-end TPS_ | 850.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 56.67 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 9.2ms |
| _P95_ | 7.9ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 166.94 tx/s |
| _Sustained TPS_ | 171.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 166.94 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.9 |
| _P99_ | 30.4ms |
| _P95_ | 30.3ms |
| _P50_ | 30.1ms |
| _Tx validation time p50 (ms)_ | 12.4 |
| _End-to-end TPS_ | 981.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.41 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 8.9ms |
| _P95_ | 7.2ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 177.87 tx/s |
| _Sustained TPS_ | 175.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 62.3 |
| _P99_ | 63.7ms |
| _P95_ | 63.6ms |
| _P50_ | 62.7ms |
| _Tx validation time p50 (ms)_ | 25.9 |
| _End-to-end TPS_ | 939.38 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.1 |
| _P99_ | 19.9ms |
| _P95_ | 18.3ms |
| _P50_ | 13.8ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 139.86 tx/s |
| _Sustained TPS_ | 138.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 139.86 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.1 |
| _P99_ | 73.3ms |
| _P95_ | 73.2ms |
| _P50_ | 72.5ms |
| _Tx validation time p50 (ms)_ | 24.9 |
| _End-to-end TPS_ | 815.64 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.19 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 24.9ms |
| _P95_ | 23.4ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 107.67 tx/s |
| _Sustained TPS_ | 106.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 62.9 |
| _P99_ | 64.3ms |
| _P95_ | 64.2ms |
| _P50_ | 63.3ms |
| _Tx validation time p50 (ms)_ | 34.8 |
| _End-to-end TPS_ | 929.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.99 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 25.5ms |
| _P95_ | 24.0ms |
| _P50_ | 18.0ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 107.77 tx/s |
| _Sustained TPS_ | 102.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.77 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 124.1 |
| _P99_ | 125.6ms |
| _P95_ | 125.3ms |
| _P50_ | 124.9ms |
| _Tx validation time p50 (ms)_ | 56.1 |
| _End-to-end TPS_ | 706.43 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.70 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.9 |
| _P99_ | 44.0ms |
| _P95_ | 33.9ms |
| _P50_ | 24.8ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 113.57 tx/s |
| _Sustained TPS_ | 114.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 76.98 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 135.2 |
| _P99_ | 138.8ms |
| _P95_ | 138.7ms |
| _P50_ | 135.9ms |
| _Tx validation time p50 (ms)_ | 65.6 |
| _End-to-end TPS_ | 646.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.37 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 46.8ms |
| _P95_ | 44.5ms |
| _P50_ | 34.1ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 86.54 tx/s |
| _Sustained TPS_ | 84.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.66 /s |
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
| _Avg. Confirmation Time (ms)_ | 130.5 |
| _P99_ | 132.8ms |
| _P95_ | 132.7ms |
| _P50_ | 132.1ms |
| _Tx validation time p50 (ms)_ | 54.6 |
| _End-to-end TPS_ | 672.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.95 /s |
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
| _Avg. Confirmation Time (ms)_ | 31.6 |
| _P99_ | 45.9ms |
| _P95_ | 39.5ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 93.77 tx/s |
| _Sustained TPS_ | 90.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 63.56 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
