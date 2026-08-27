--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-27 14:09:39.226250548 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1206.42 | n/a | 24.1 | 24.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 208.47 | 205.89 | 4.7 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 946.08 | n/a | 31.0 | 31.4 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 166.54 | 177.26 | 5.9 | 7.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1170.39 | n/a | 24.8 | 25.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 181.81 | 187.23 | 5.4 | 6.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1185.57 | n/a | 48.8 | 50.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 147.11 | 143.96 | 13.4 | 17.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 948.98 | n/a | 61.2 | 62.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 107.02 | 104.29 | 18.4 | 22.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1025.39 | n/a | 56.6 | 58.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 111.91 | 105.47 | 17.7 | 24.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 746.88 | n/a | 117.0 | 118.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 123.08 | 121.55 | 24.1 | 32.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 677.20 | n/a | 128.4 | 130.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.71 | 86.21 | 33.6 | 41.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 649.49 | n/a | 135.9 | 137.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 95.98 | 92.94 | 31.0 | 40.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.1 |
| _P99_ | 24.6ms |
| _P95_ | 24.6ms |
| _P50_ | 24.3ms |
| _Tx validation time p50 (ms)_ | 16.8 |
| _End-to-end TPS_ | 1206.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 80.43 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 10.5ms |
| _P95_ | 5.9ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 208.47 tx/s |
| _Sustained TPS_ | 205.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 208.47 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 31.5ms |
| _P95_ | 31.4ms |
| _P50_ | 31.2ms |
| _Tx validation time p50 (ms)_ | 14.1 |
| _End-to-end TPS_ | 946.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.07 /s |
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
| _P99_ | 10.8ms |
| _P95_ | 7.8ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 166.54 tx/s |
| _Sustained TPS_ | 177.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 166.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.8 |
| _P99_ | 25.4ms |
| _P95_ | 25.4ms |
| _P50_ | 25.0ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 1170.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 78.03 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 10.7ms |
| _P95_ | 6.3ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 181.81 tx/s |
| _Sustained TPS_ | 187.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 181.81 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 48.8 |
| _P99_ | 50.4ms |
| _P95_ | 50.3ms |
| _P50_ | 49.4ms |
| _Tx validation time p50 (ms)_ | 23.0 |
| _End-to-end TPS_ | 1185.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.52 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.4 |
| _P99_ | 23.2ms |
| _P95_ | 17.8ms |
| _P50_ | 13.1ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 147.11 tx/s |
| _Sustained TPS_ | 143.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 147.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.2 |
| _P99_ | 62.8ms |
| _P95_ | 62.4ms |
| _P50_ | 61.6ms |
| _Tx validation time p50 (ms)_ | 22.7 |
| _End-to-end TPS_ | 948.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.63 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 23.7ms |
| _P95_ | 22.4ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 107.02 tx/s |
| _Sustained TPS_ | 104.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 107.02 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 56.6 |
| _P99_ | 58.3ms |
| _P95_ | 58.2ms |
| _P50_ | 56.7ms |
| _Tx validation time p50 (ms)_ | 22.9 |
| _End-to-end TPS_ | 1025.39 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 34.18 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.7 |
| _P99_ | 24.9ms |
| _P95_ | 24.0ms |
| _P50_ | 17.7ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 111.91 tx/s |
| _Sustained TPS_ | 105.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 111.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 117.0 |
| _P99_ | 118.9ms |
| _P95_ | 118.7ms |
| _P50_ | 117.9ms |
| _Tx validation time p50 (ms)_ | 47.6 |
| _End-to-end TPS_ | 746.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.60 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.1 |
| _P99_ | 35.4ms |
| _P95_ | 32.4ms |
| _P50_ | 23.2ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 123.08 tx/s |
| _Sustained TPS_ | 121.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 83.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.4 |
| _P99_ | 131.0ms |
| _P95_ | 130.7ms |
| _P50_ | 129.7ms |
| _Tx validation time p50 (ms)_ | 65.1 |
| _End-to-end TPS_ | 677.20 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.05 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.6 |
| _P99_ | 47.3ms |
| _P95_ | 41.0ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 87.71 tx/s |
| _Sustained TPS_ | 86.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 60.42 /s |
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
| _Avg. Confirmation Time (ms)_ | 135.9 |
| _P99_ | 137.3ms |
| _P95_ | 137.1ms |
| _P50_ | 136.3ms |
| _Tx validation time p50 (ms)_ | 50.7 |
| _End-to-end TPS_ | 649.49 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.43 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 44.3ms |
| _P95_ | 40.5ms |
| _P50_ | 30.9ms |
| _Tx validation time p50 (ms)_ | 9.1 |
| _End-to-end TPS_ | 95.98 tx/s |
| _Sustained TPS_ | 92.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 65.06 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
