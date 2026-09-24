--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-24 15:11:25.117255074 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 960.40 | n/a | 30.4 | 30.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 209.34 | 209.16 | 4.7 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 685.31 | n/a | 42.8 | 43.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 172.35 | 171.50 | 5.7 | 6.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 912.61 | n/a | 32.0 | 32.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 182.01 | 177.84 | 5.4 | 6.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 794.13 | n/a | 74.1 | 74.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 131.90 | 128.39 | 14.9 | 19.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 668.89 | n/a | 87.7 | 88.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 102.16 | 100.44 | 19.3 | 25.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 787.79 | n/a | 73.8 | 75.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 103.76 | 99.80 | 19.1 | 24.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 626.48 | n/a | 140.2 | 143.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 112.43 | 111.15 | 26.4 | 30.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 642.33 | n/a | 136.1 | 138.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.10 | 83.31 | 35.4 | 41.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 652.90 | n/a | 134.0 | 137.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.77 | 87.30 | 32.9 | 39.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.4 |
| _P99_ | 31.0ms |
| _P95_ | 30.9ms |
| _P50_ | 30.7ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 960.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.03 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 5.7ms |
| _P95_ | 5.7ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 209.34 tx/s |
| _Sustained TPS_ | 209.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 209.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 42.8 |
| _P99_ | 43.6ms |
| _P95_ | 43.6ms |
| _P50_ | 43.2ms |
| _Tx validation time p50 (ms)_ | 11.8 |
| _End-to-end TPS_ | 685.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 45.69 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.1ms |
| _P95_ | 6.5ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 172.35 tx/s |
| _Sustained TPS_ | 171.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 172.35 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.0 |
| _P99_ | 32.7ms |
| _P95_ | 32.6ms |
| _P50_ | 32.3ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 912.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.84 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 9.3ms |
| _P95_ | 6.4ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 182.01 tx/s |
| _Sustained TPS_ | 177.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 182.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.1 |
| _P99_ | 74.8ms |
| _P95_ | 74.8ms |
| _P50_ | 74.4ms |
| _Tx validation time p50 (ms)_ | 26.0 |
| _End-to-end TPS_ | 794.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 22.6ms |
| _P95_ | 19.8ms |
| _P50_ | 14.3ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 131.90 tx/s |
| _Sustained TPS_ | 128.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 131.90 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 87.7 |
| _P99_ | 89.1ms |
| _P95_ | 88.6ms |
| _P50_ | 88.1ms |
| _Tx validation time p50 (ms)_ | 32.3 |
| _End-to-end TPS_ | 668.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.30 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 29.9ms |
| _P95_ | 25.4ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 102.16 tx/s |
| _Sustained TPS_ | 100.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.16 /s |
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
| _Avg. Confirmation Time (ms)_ | 73.8 |
| _P99_ | 75.9ms |
| _P95_ | 75.9ms |
| _P50_ | 73.9ms |
| _Tx validation time p50 (ms)_ | 26.5 |
| _End-to-end TPS_ | 787.79 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.26 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.1 |
| _P99_ | 33.0ms |
| _P95_ | 24.4ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 103.76 tx/s |
| _Sustained TPS_ | 99.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.76 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.2 |
| _P99_ | 143.6ms |
| _P95_ | 143.5ms |
| _P50_ | 141.5ms |
| _Tx validation time p50 (ms)_ | 44.9 |
| _End-to-end TPS_ | 626.48 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.92 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.4 |
| _P99_ | 33.3ms |
| _P95_ | 30.7ms |
| _P50_ | 26.6ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 112.43 tx/s |
| _Sustained TPS_ | 111.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 74.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 136.1 |
| _P99_ | 138.8ms |
| _P95_ | 138.6ms |
| _P50_ | 137.5ms |
| _Tx validation time p50 (ms)_ | 57.7 |
| _End-to-end TPS_ | 642.33 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.27 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.4 |
| _P99_ | 43.9ms |
| _P95_ | 41.9ms |
| _P50_ | 35.3ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 84.10 tx/s |
| _Sustained TPS_ | 83.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.07 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 134.0 |
| _P99_ | 137.6ms |
| _P95_ | 137.4ms |
| _P50_ | 134.3ms |
| _Tx validation time p50 (ms)_ | 47.3 |
| _End-to-end TPS_ | 652.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.51 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 47.1ms |
| _P95_ | 39.7ms |
| _P50_ | 32.9ms |
| _Tx validation time p50 (ms)_ | 8.1 |
| _End-to-end TPS_ | 89.77 tx/s |
| _Sustained TPS_ | 87.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.85 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
