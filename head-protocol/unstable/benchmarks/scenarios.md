--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-07 09:38:01.024074306 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1060.17 | n/a | 27.6 | 28.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 210.17 | 208.63 | 4.7 | 6.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 883.54 | n/a | 32.9 | 33.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 172.32 | 169.83 | 5.7 | 7.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 922.15 | n/a | 31.6 | 32.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 186.63 | 184.75 | 5.3 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 915.90 | n/a | 64.0 | 64.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 148.24 | 147.70 | 13.3 | 15.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 795.26 | n/a | 73.5 | 75.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 107.54 | 104.85 | 18.4 | 21.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 881.61 | n/a | 65.9 | 67.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 111.42 | 108.15 | 17.6 | 22.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 653.97 | n/a | 134.3 | 135.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 113.34 | 112.29 | 25.9 | 32.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 617.05 | n/a | 142.3 | 144.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 89.54 | 87.41 | 33.1 | 43.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 686.28 | n/a | 127.9 | 130.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 95.86 | 92.93 | 30.7 | 41.2 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.6 |
| _P99_ | 28.2ms |
| _P95_ | 28.2ms |
| _P50_ | 27.8ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 1060.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.68 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 6.1ms |
| _P95_ | 6.0ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 210.17 tx/s |
| _Sustained TPS_ | 208.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 210.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 33.8ms |
| _P95_ | 33.7ms |
| _P50_ | 33.2ms |
| _Tx validation time p50 (ms)_ | 12.1 |
| _End-to-end TPS_ | 883.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.90 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 8.8ms |
| _P95_ | 7.1ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 172.32 tx/s |
| _Sustained TPS_ | 169.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 172.32 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.6 |
| _P99_ | 32.3ms |
| _P95_ | 32.3ms |
| _P50_ | 31.9ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 922.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.48 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 7.4ms |
| _P95_ | 6.7ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 186.63 tx/s |
| _Sustained TPS_ | 184.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 186.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.0 |
| _P99_ | 65.1ms |
| _P95_ | 64.7ms |
| _P50_ | 64.3ms |
| _Tx validation time p50 (ms)_ | 24.0 |
| _End-to-end TPS_ | 915.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.53 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.3 |
| _P99_ | 17.3ms |
| _P95_ | 15.5ms |
| _P50_ | 13.4ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 148.24 tx/s |
| _Sustained TPS_ | 147.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 148.24 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.5 |
| _P99_ | 75.2ms |
| _P95_ | 75.1ms |
| _P50_ | 74.0ms |
| _Tx validation time p50 (ms)_ | 22.7 |
| _End-to-end TPS_ | 795.26 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.51 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 23.0ms |
| _P95_ | 21.6ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 107.54 tx/s |
| _Sustained TPS_ | 104.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.9 |
| _P99_ | 67.8ms |
| _P95_ | 67.7ms |
| _P50_ | 66.5ms |
| _Tx validation time p50 (ms)_ | 23.6 |
| _End-to-end TPS_ | 881.61 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.39 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.6 |
| _P99_ | 23.0ms |
| _P95_ | 22.9ms |
| _P50_ | 17.9ms |
| _Tx validation time p50 (ms)_ | 5.0 |
| _End-to-end TPS_ | 111.42 tx/s |
| _Sustained TPS_ | 108.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 111.42 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 134.3 |
| _P99_ | 135.9ms |
| _P95_ | 135.8ms |
| _P50_ | 135.1ms |
| _Tx validation time p50 (ms)_ | 47.5 |
| _End-to-end TPS_ | 653.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.53 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.9 |
| _P99_ | 38.3ms |
| _P95_ | 32.1ms |
| _P50_ | 25.4ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 113.34 tx/s |
| _Sustained TPS_ | 112.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 78.08 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 142.3 |
| _P99_ | 144.6ms |
| _P95_ | 144.5ms |
| _P50_ | 143.9ms |
| _Tx validation time p50 (ms)_ | 48.0 |
| _End-to-end TPS_ | 617.05 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.71 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.1 |
| _P99_ | 45.9ms |
| _P95_ | 43.5ms |
| _P50_ | 32.3ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 89.54 tx/s |
| _Sustained TPS_ | 87.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.69 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 127.9 |
| _P99_ | 130.6ms |
| _P95_ | 130.5ms |
| _P50_ | 129.5ms |
| _Tx validation time p50 (ms)_ | 45.4 |
| _End-to-end TPS_ | 686.28 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.25 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 30.7 |
| _P99_ | 43.4ms |
| _P95_ | 41.2ms |
| _P50_ | 30.8ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 95.86 tx/s |
| _Sustained TPS_ | 92.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 66.04 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
