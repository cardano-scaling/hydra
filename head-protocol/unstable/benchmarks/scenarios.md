--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-18 14:10:16.653524686 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.8 | 39.60 | n/a | 753.8 | 757.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 1.3 | 23.91 | 32.64 | 41.7 | 120.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 266.72 | n/a | 111.5 | 112.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 3.1 | 9.80 | 10.67 | 101.9 | 325.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 871.15 | n/a | 33.9 | 34.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 2.9 | 10.31 | 10.24 | 96.9 | 310.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.4 | 170.66 | n/a | 350.2 | 351.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 3.5 | 16.98 | 18.93 | 115.8 | 320.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.7 | 91.54 | n/a | 651.6 | 655.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 2.3 | 26.01 | 38.37 | 76.2 | 299.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.5 | 114.41 | n/a | 484.6 | 523.8 |
| Nodes=2, Mixed, wait for tx valid | 60 | 5.4 | 11.07 | 12.92 | 179.7 | 538.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 929.86 | n/a | 94.8 | 96.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 108.21 | 102.68 | 27.6 | 68.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 480.02 | n/a | 183.8 | 186.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 2.0 | 46.09 | 40.82 | 64.2 | 227.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 957.89 | n/a | 91.8 | 93.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 94.20 | 91.25 | 31.3 | 73.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 753.8 |
| _P99_ | 757.6ms |
| _P95_ | 757.5ms |
| _P50_ | 757.4ms |
| _Tx validation time p50 (ms)_ | 515.5 |
| _End-to-end TPS_ | 39.60 tx/s |
| _Backlog drain time (s)_ | 0.8 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.64 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.7 |
| _P99_ | 139.7ms |
| _P95_ | 120.6ms |
| _P50_ | 17.4ms |
| _Tx validation time p50 (ms)_ | 2.4 |
| _End-to-end TPS_ | 23.91 tx/s |
| _Sustained TPS_ | 32.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 23.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 111.5 |
| _P99_ | 112.2ms |
| _P95_ | 112.2ms |
| _P50_ | 112.1ms |
| _Tx validation time p50 (ms)_ | 14.1 |
| _End-to-end TPS_ | 266.72 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.78 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 101.9 |
| _P99_ | 372.2ms |
| _P95_ | 325.8ms |
| _P50_ | 59.7ms |
| _Tx validation time p50 (ms)_ | 2.5 |
| _End-to-end TPS_ | 9.80 tx/s |
| _Sustained TPS_ | 10.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 9.80 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 34.2ms |
| _P95_ | 34.2ms |
| _P50_ | 34.1ms |
| _Tx validation time p50 (ms)_ | 21.6 |
| _End-to-end TPS_ | 871.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.08 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 96.9 |
| _P99_ | 336.9ms |
| _P95_ | 310.9ms |
| _P50_ | 59.2ms |
| _Tx validation time p50 (ms)_ | 2.5 |
| _End-to-end TPS_ | 10.31 tx/s |
| _Sustained TPS_ | 10.24 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 10.31 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 350.2 |
| _P99_ | 351.4ms |
| _P95_ | 351.3ms |
| _P50_ | 350.5ms |
| _Tx validation time p50 (ms)_ | 247.7 |
| _End-to-end TPS_ | 170.66 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.69 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 115.8 |
| _P99_ | 436.8ms |
| _P95_ | 320.4ms |
| _P50_ | 81.6ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 16.98 tx/s |
| _Sustained TPS_ | 18.93 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 16.98 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 651.6 |
| _P99_ | 655.2ms |
| _P95_ | 655.1ms |
| _P50_ | 654.6ms |
| _Tx validation time p50 (ms)_ | 309.8 |
| _End-to-end TPS_ | 91.54 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.05 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 76.2 |
| _P99_ | 359.8ms |
| _P95_ | 299.3ms |
| _P50_ | 37.8ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 26.01 tx/s |
| _Sustained TPS_ | 38.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 26.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 484.6 |
| _P99_ | 524.1ms |
| _P95_ | 523.8ms |
| _P50_ | 488.0ms |
| _Tx validation time p50 (ms)_ | 56.0 |
| _End-to-end TPS_ | 114.41 tx/s |
| _Backlog drain time (s)_ | 0.5 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 3.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 179.7 |
| _P99_ | 639.1ms |
| _P95_ | 538.1ms |
| _P50_ | 119.9ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 11.07 tx/s |
| _Sustained TPS_ | 12.92 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 11.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 94.8 |
| _P99_ | 96.0ms |
| _P95_ | 96.0ms |
| _P50_ | 95.6ms |
| _Tx validation time p50 (ms)_ | 25.3 |
| _End-to-end TPS_ | 929.86 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.66 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.6 |
| _P99_ | 81.7ms |
| _P95_ | 68.1ms |
| _P50_ | 23.0ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 108.21 tx/s |
| _Sustained TPS_ | 102.68 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 72.14 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 183.8 |
| _P99_ | 186.8ms |
| _P95_ | 186.8ms |
| _P50_ | 186.3ms |
| _Tx validation time p50 (ms)_ | 36.3 |
| _End-to-end TPS_ | 480.02 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.67 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 64.2 |
| _P99_ | 391.4ms |
| _P95_ | 227.0ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 46.09 tx/s |
| _Sustained TPS_ | 40.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 31.75 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 91.8 |
| _P99_ | 93.5ms |
| _P95_ | 93.4ms |
| _P50_ | 92.9ms |
| _Tx validation time p50 (ms)_ | 28.5 |
| _End-to-end TPS_ | 957.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.29 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 83.2ms |
| _P95_ | 73.9ms |
| _P50_ | 26.9ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 94.20 tx/s |
| _Sustained TPS_ | 91.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 63.84 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
