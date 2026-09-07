--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-07 10:10:59.7333756 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1942.10 | n/a | 15.0 | 15.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 263.37 | 266.74 | 3.7 | 6.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1235.54 | n/a | 23.7 | 24.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 226.06 | 231.35 | 4.4 | 6.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1203.91 | n/a | 24.3 | 24.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 238.42 | 233.74 | 4.1 | 5.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1468.55 | n/a | 39.7 | 40.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 197.45 | 206.04 | 10.0 | 14.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.0 | 1399.51 | n/a | 41.8 | 42.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.3 | 47.56 | 74.51 | 41.8 | 247.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1474.67 | n/a | 39.6 | 40.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.3 | 172.00 | 168.48 | 11.5 | 15.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1211.40 | n/a | 72.8 | 74.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 129.03 | 146.92 | 22.9 | 28.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 992.68 | n/a | 88.6 | 90.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 114.32 | 111.94 | 25.8 | 33.1 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 1031.06 | n/a | 85.9 | 87.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.8 | 107.12 | 106.24 | 27.7 | 88.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 15.0 |
| _P99_ | 15.4ms |
| _P95_ | 15.3ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 1942.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 129.47 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 3.7 |
| _P99_ | 6.7ms |
| _P95_ | 6.2ms |
| _P50_ | 3.3ms |
| _Tx validation time p50 (ms)_ | 1.1 |
| _End-to-end TPS_ | 263.37 tx/s |
| _Sustained TPS_ | 266.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 263.37 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 23.7 |
| _P99_ | 24.1ms |
| _P95_ | 24.1ms |
| _P50_ | 23.9ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 1235.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 82.37 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 8.2ms |
| _P95_ | 6.4ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 226.06 tx/s |
| _Sustained TPS_ | 231.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 226.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.3 |
| _P99_ | 24.8ms |
| _P95_ | 24.7ms |
| _P50_ | 24.5ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 1203.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 80.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.1 |
| _P99_ | 6.1ms |
| _P95_ | 5.8ms |
| _P50_ | 4.0ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 238.42 tx/s |
| _Sustained TPS_ | 233.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 238.42 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 39.7 |
| _P99_ | 40.6ms |
| _P95_ | 40.6ms |
| _P50_ | 39.9ms |
| _Tx validation time p50 (ms)_ | 15.1 |
| _End-to-end TPS_ | 1468.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.95 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 10.0 |
| _P99_ | 16.5ms |
| _P95_ | 14.9ms |
| _P50_ | 9.1ms |
| _Tx validation time p50 (ms)_ | 3.1 |
| _End-to-end TPS_ | 197.45 tx/s |
| _Sustained TPS_ | 206.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 197.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 41.8 |
| _P99_ | 42.3ms |
| _P95_ | 42.3ms |
| _P50_ | 42.0ms |
| _Tx validation time p50 (ms)_ | 25.0 |
| _End-to-end TPS_ | 1399.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 46.65 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.8 |
| _P99_ | 281.7ms |
| _P95_ | 247.4ms |
| _P50_ | 12.7ms |
| _Tx validation time p50 (ms)_ | 3.5 |
| _End-to-end TPS_ | 47.56 tx/s |
| _Sustained TPS_ | 74.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 47.56 /s |
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
| _Avg. Confirmation Time (ms)_ | 39.6 |
| _P99_ | 40.3ms |
| _P95_ | 40.0ms |
| _P50_ | 39.7ms |
| _Tx validation time p50 (ms)_ | 15.9 |
| _End-to-end TPS_ | 1474.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 49.16 /s |
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
| _Avg. Confirmation Time (ms)_ | 11.5 |
| _P99_ | 15.6ms |
| _P95_ | 15.0ms |
| _P50_ | 11.6ms |
| _Tx validation time p50 (ms)_ | 3.1 |
| _End-to-end TPS_ | 172.00 tx/s |
| _Sustained TPS_ | 168.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 172.00 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.8 |
| _P99_ | 74.2ms |
| _P95_ | 74.2ms |
| _P50_ | 72.6ms |
| _Tx validation time p50 (ms)_ | 20.6 |
| _End-to-end TPS_ | 1211.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.92 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.9 |
| _P99_ | 91.6ms |
| _P95_ | 28.4ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 129.03 tx/s |
| _Sustained TPS_ | 146.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 87.45 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 88.6 |
| _P99_ | 90.5ms |
| _P95_ | 90.4ms |
| _P50_ | 88.4ms |
| _Tx validation time p50 (ms)_ | 23.4 |
| _End-to-end TPS_ | 992.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.06 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.8 |
| _P99_ | 36.1ms |
| _P95_ | 33.1ms |
| _P50_ | 26.0ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 114.32 tx/s |
| _Sustained TPS_ | 111.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 77.48 /s |
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
| _Avg. Confirmation Time (ms)_ | 85.9 |
| _P99_ | 87.1ms |
| _P95_ | 87.1ms |
| _P50_ | 86.2ms |
| _Tx validation time p50 (ms)_ | 30.7 |
| _End-to-end TPS_ | 1031.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.91 /s |
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
| _Avg. Confirmation Time (ms)_ | 27.7 |
| _P99_ | 143.1ms |
| _P95_ | 88.5ms |
| _P50_ | 20.0ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 107.12 tx/s |
| _Sustained TPS_ | 106.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.60 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
