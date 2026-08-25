--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-25 12:32:38.753623951 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1317.73 | n/a | 22.3 | 22.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.3 | 100.55 | 90.40 | 9.9 | 39.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 552.62 | n/a | 53.8 | 54.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.4 | 68.88 | 114.47 | 14.5 | 57.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 588.99 | n/a | 50.4 | 50.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 1.7 | 17.77 | 16.80 | 56.2 | 243.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.2 | 321.31 | n/a | 184.2 | 185.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.7 | 84.82 | 74.99 | 23.4 | 132.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 643.82 | n/a | 92.2 | 92.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 2.2 | 26.99 | 36.66 | 73.9 | 262.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 565.65 | n/a | 104.8 | 105.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 1.4 | 42.47 | 40.10 | 44.9 | 207.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1097.40 | n/a | 79.6 | 80.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 2.0 | 45.63 | 43.50 | 65.2 | 263.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.3 | 285.00 | n/a | 313.1 | 314.9 |
| Nodes=3, Growing, wait for tx valid | 90 | 4.6 | 19.72 | 16.95 | 151.1 | 544.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 942.71 | n/a | 94.2 | 94.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 95.81 | 87.38 | 31.1 | 62.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 22.3 |
| _P99_ | 22.6ms |
| _P95_ | 22.6ms |
| _P50_ | 22.4ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 1317.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 87.85 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.9 |
| _P99_ | 74.8ms |
| _P95_ | 39.0ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 100.55 tx/s |
| _Sustained TPS_ | 90.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 100.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 53.8 |
| _P99_ | 54.2ms |
| _P95_ | 54.2ms |
| _P50_ | 54.0ms |
| _Tx validation time p50 (ms)_ | 42.9 |
| _End-to-end TPS_ | 552.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 36.84 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.5 |
| _P99_ | 172.9ms |
| _P95_ | 57.1ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 68.88 tx/s |
| _Sustained TPS_ | 114.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 68.88 /s |
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
| _Avg. Confirmation Time (ms)_ | 50.4 |
| _P99_ | 50.8ms |
| _P95_ | 50.7ms |
| _P50_ | 50.5ms |
| _Tx validation time p50 (ms)_ | 43.0 |
| _End-to-end TPS_ | 588.99 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.27 /s |
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
| _Avg. Confirmation Time (ms)_ | 56.2 |
| _P99_ | 310.1ms |
| _P95_ | 243.6ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 17.77 tx/s |
| _Sustained TPS_ | 16.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 17.77 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 184.2 |
| _P99_ | 185.6ms |
| _P95_ | 185.6ms |
| _P50_ | 184.7ms |
| _Tx validation time p50 (ms)_ | 98.2 |
| _End-to-end TPS_ | 321.31 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 16.07 /s |
| _Avg txs per snapshot_ | 20.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.4 |
| _P99_ | 206.5ms |
| _P95_ | 132.7ms |
| _P50_ | 9.5ms |
| _Tx validation time p50 (ms)_ | 2.6 |
| _End-to-end TPS_ | 84.82 tx/s |
| _Sustained TPS_ | 74.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 84.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 92.2 |
| _P99_ | 93.0ms |
| _P95_ | 92.9ms |
| _P50_ | 92.3ms |
| _Tx validation time p50 (ms)_ | 71.8 |
| _End-to-end TPS_ | 643.82 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.46 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 73.9 |
| _P99_ | 351.0ms |
| _P95_ | 262.0ms |
| _P50_ | 15.8ms |
| _Tx validation time p50 (ms)_ | 4.1 |
| _End-to-end TPS_ | 26.99 tx/s |
| _Sustained TPS_ | 36.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 26.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 104.8 |
| _P99_ | 105.6ms |
| _P95_ | 105.3ms |
| _P50_ | 104.9ms |
| _Tx validation time p50 (ms)_ | 86.9 |
| _End-to-end TPS_ | 565.65 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.86 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 44.9 |
| _P99_ | 214.5ms |
| _P95_ | 207.4ms |
| _P50_ | 13.2ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 42.47 tx/s |
| _Sustained TPS_ | 40.10 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 42.47 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.6 |
| _P99_ | 81.0ms |
| _P95_ | 80.8ms |
| _P50_ | 79.5ms |
| _Tx validation time p50 (ms)_ | 28.2 |
| _End-to-end TPS_ | 1097.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.39 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 65.2 |
| _P99_ | 342.3ms |
| _P95_ | 263.9ms |
| _P50_ | 33.6ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 45.63 tx/s |
| _Sustained TPS_ | 43.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 31.43 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 313.1 |
| _P99_ | 315.1ms |
| _P95_ | 314.9ms |
| _P50_ | 312.9ms |
| _Tx validation time p50 (ms)_ | 69.4 |
| _End-to-end TPS_ | 285.00 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 6.33 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 151.1 |
| _P99_ | 954.5ms |
| _P95_ | 544.9ms |
| _P50_ | 70.8ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 19.72 tx/s |
| _Sustained TPS_ | 16.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 63 |
| _Snapshots per second_ | 13.81 /s |
| _Avg txs per snapshot_ | 1.4 |
| _Peak node RSS (MB)_ | 146.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 94.2 |
| _P99_ | 94.7ms |
| _P95_ | 94.7ms |
| _P50_ | 94.3ms |
| _Tx validation time p50 (ms)_ | 33.5 |
| _End-to-end TPS_ | 942.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.95 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 87.3ms |
| _P95_ | 62.6ms |
| _P50_ | 25.3ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 95.81 tx/s |
| _Sustained TPS_ | 87.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.94 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
