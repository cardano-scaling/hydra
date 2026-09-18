--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-18 10:21:07.46360501 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1350.73 | n/a | 21.6 | 22.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.7 | 41.01 | 73.15 | 24.3 | 126.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 460.47 | n/a | 64.5 | 64.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.5 | 58.16 | 50.45 | 17.1 | 26.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.2 | 160.86 | n/a | 185.5 | 186.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.5 | 64.40 | 97.70 | 15.5 | 68.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.3 | 188.62 | n/a | 316.8 | 317.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.7 | 89.24 | 91.32 | 22.1 | 70.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 492.33 | n/a | 120.5 | 121.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.7 | 36.08 | 67.36 | 55.2 | 272.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.4 | 146.90 | n/a | 404.2 | 408.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.9 | 69.74 | 67.81 | 28.5 | 110.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.4 | 229.69 | n/a | 388.9 | 391.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.4 | 62.11 | 78.68 | 47.8 | 120.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 698.17 | n/a | 126.3 | 127.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 2.4 | 37.59 | 34.90 | 76.9 | 245.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 645.25 | n/a | 137.3 | 139.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 2.9 | 31.03 | 30.53 | 96.4 | 256.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 21.6 |
| _P99_ | 22.0ms |
| _P95_ | 22.0ms |
| _P50_ | 21.7ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 1350.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 90.05 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.3 |
| _P99_ | 242.8ms |
| _P95_ | 126.2ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 41.01 tx/s |
| _Sustained TPS_ | 73.15 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 41.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 64.5 |
| _P99_ | 65.0ms |
| _P95_ | 64.9ms |
| _P50_ | 64.7ms |
| _Tx validation time p50 (ms)_ | 47.2 |
| _End-to-end TPS_ | 460.47 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.70 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.1 |
| _P99_ | 227.1ms |
| _P95_ | 26.4ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 58.16 tx/s |
| _Sustained TPS_ | 50.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 58.16 /s |
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
| _Avg. Confirmation Time (ms)_ | 185.5 |
| _P99_ | 186.3ms |
| _P95_ | 186.3ms |
| _P50_ | 185.7ms |
| _Tx validation time p50 (ms)_ | 104.0 |
| _End-to-end TPS_ | 160.86 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 10.72 /s |
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
| _Avg. Confirmation Time (ms)_ | 15.5 |
| _P99_ | 173.1ms |
| _P95_ | 68.2ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 64.40 tx/s |
| _Sustained TPS_ | 97.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 64.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 316.8 |
| _P99_ | 317.9ms |
| _P95_ | 317.9ms |
| _P50_ | 317.0ms |
| _Tx validation time p50 (ms)_ | 283.5 |
| _End-to-end TPS_ | 188.62 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 6.29 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.1 |
| _P99_ | 77.2ms |
| _P95_ | 70.8ms |
| _P50_ | 13.0ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 89.24 tx/s |
| _Sustained TPS_ | 91.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 89.24 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 120.5 |
| _P99_ | 121.8ms |
| _P95_ | 121.7ms |
| _P50_ | 120.5ms |
| _Tx validation time p50 (ms)_ | 69.7 |
| _End-to-end TPS_ | 492.33 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.41 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 55.2 |
| _P99_ | 392.6ms |
| _P95_ | 272.2ms |
| _P50_ | 16.7ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 36.08 tx/s |
| _Sustained TPS_ | 67.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 36.08 /s |
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
| _Avg. Confirmation Time (ms)_ | 404.2 |
| _P99_ | 408.2ms |
| _P95_ | 408.1ms |
| _P50_ | 407.4ms |
| _Tx validation time p50 (ms)_ | 182.3 |
| _End-to-end TPS_ | 146.90 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 4.90 /s |
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
| _Avg. Confirmation Time (ms)_ | 28.5 |
| _P99_ | 143.6ms |
| _P95_ | 110.0ms |
| _P50_ | 16.9ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 69.74 tx/s |
| _Sustained TPS_ | 67.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 69.74 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 388.9 |
| _P99_ | 391.3ms |
| _P95_ | 391.2ms |
| _P50_ | 389.8ms |
| _Tx validation time p50 (ms)_ | 173.1 |
| _End-to-end TPS_ | 229.69 tx/s |
| _Backlog drain time (s)_ | 0.4 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.10 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 47.8 |
| _P99_ | 222.1ms |
| _P95_ | 120.8ms |
| _P50_ | 25.7ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 62.11 tx/s |
| _Sustained TPS_ | 78.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 42.10 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 126.3 |
| _P99_ | 127.8ms |
| _P95_ | 127.7ms |
| _P50_ | 126.7ms |
| _Tx validation time p50 (ms)_ | 41.5 |
| _End-to-end TPS_ | 698.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.51 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 76.9 |
| _P99_ | 319.7ms |
| _P95_ | 245.7ms |
| _P50_ | 35.9ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 37.59 tx/s |
| _Sustained TPS_ | 34.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 25.48 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 137.3 |
| _P99_ | 139.2ms |
| _P95_ | 139.1ms |
| _P50_ | 137.5ms |
| _Tx validation time p50 (ms)_ | 39.7 |
| _End-to-end TPS_ | 645.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.34 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 96.4 |
| _P99_ | 384.3ms |
| _P95_ | 256.5ms |
| _P50_ | 74.7ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 31.03 tx/s |
| _Sustained TPS_ | 30.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 21.03 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
