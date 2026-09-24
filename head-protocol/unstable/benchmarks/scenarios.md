--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-24 14:01:27.429773346 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1709.24 | n/a | 17.0 | 17.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 268.70 | 274.58 | 3.7 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1165.35 | n/a | 25.2 | 25.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 229.12 | 228.16 | 4.3 | 5.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1580.82 | n/a | 18.5 | 18.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 241.95 | 238.92 | 4.1 | 6.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1752.51 | n/a | 33.4 | 34.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 200.26 | 196.49 | 9.9 | 16.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.0 | 1255.25 | n/a | 46.9 | 47.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.4 | 163.20 | 158.94 | 12.1 | 15.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1368.84 | n/a | 42.9 | 43.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.3 | 172.73 | 168.63 | 11.4 | 15.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1115.85 | n/a | 79.3 | 79.6 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.6 | 150.38 | 158.08 | 19.7 | 30.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 926.55 | n/a | 95.0 | 95.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.7 | 124.72 | 125.24 | 23.6 | 30.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 1019.22 | n/a | 86.4 | 88.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.7 | 128.50 | 124.23 | 23.1 | 30.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 17.0 |
| _P99_ | 17.4ms |
| _P95_ | 17.3ms |
| _P50_ | 17.1ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 1709.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 113.95 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 3.7 |
| _P99_ | 6.3ms |
| _P95_ | 5.9ms |
| _P50_ | 3.3ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 268.70 tx/s |
| _Sustained TPS_ | 274.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 268.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 25.6ms |
| _P95_ | 25.6ms |
| _P50_ | 25.4ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 1165.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.69 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 8.3ms |
| _P95_ | 5.2ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 229.12 tx/s |
| _Sustained TPS_ | 228.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 229.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 18.5 |
| _P99_ | 18.8ms |
| _P95_ | 18.8ms |
| _P50_ | 18.6ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 1580.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 105.39 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.1 |
| _P99_ | 6.3ms |
| _P95_ | 6.0ms |
| _P50_ | 3.7ms |
| _Tx validation time p50 (ms)_ | 1.2 |
| _End-to-end TPS_ | 241.95 tx/s |
| _Sustained TPS_ | 238.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 241.95 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 34.0ms |
| _P95_ | 34.0ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 14.6 |
| _End-to-end TPS_ | 1752.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.42 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 9.9 |
| _P99_ | 18.5ms |
| _P95_ | 16.3ms |
| _P50_ | 8.9ms |
| _Tx validation time p50 (ms)_ | 2.3 |
| _End-to-end TPS_ | 200.26 tx/s |
| _Sustained TPS_ | 196.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 200.26 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 46.9 |
| _P99_ | 47.6ms |
| _P95_ | 47.5ms |
| _P50_ | 47.1ms |
| _Tx validation time p50 (ms)_ | 18.1 |
| _End-to-end TPS_ | 1255.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 41.84 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.1 |
| _P99_ | 24.4ms |
| _P95_ | 15.4ms |
| _P50_ | 11.4ms |
| _Tx validation time p50 (ms)_ | 3.4 |
| _End-to-end TPS_ | 163.20 tx/s |
| _Sustained TPS_ | 158.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 163.20 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 42.9 |
| _P99_ | 43.3ms |
| _P95_ | 43.3ms |
| _P50_ | 43.0ms |
| _Tx validation time p50 (ms)_ | 15.4 |
| _End-to-end TPS_ | 1368.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 45.63 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.4 |
| _P99_ | 17.0ms |
| _P95_ | 15.7ms |
| _P50_ | 11.1ms |
| _Tx validation time p50 (ms)_ | 3.3 |
| _End-to-end TPS_ | 172.73 tx/s |
| _Sustained TPS_ | 168.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 172.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.3 |
| _P99_ | 79.7ms |
| _P95_ | 79.6ms |
| _P50_ | 79.4ms |
| _Tx validation time p50 (ms)_ | 24.5 |
| _End-to-end TPS_ | 1115.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.80 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.7 |
| _P99_ | 34.6ms |
| _P95_ | 30.8ms |
| _P50_ | 18.8ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 150.38 tx/s |
| _Sustained TPS_ | 158.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 101.92 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 95.0 |
| _P99_ | 96.0ms |
| _P95_ | 95.8ms |
| _P50_ | 95.3ms |
| _Tx validation time p50 (ms)_ | 27.8 |
| _End-to-end TPS_ | 926.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.59 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.6 |
| _P99_ | 32.8ms |
| _P95_ | 30.3ms |
| _P50_ | 23.5ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 124.72 tx/s |
| _Sustained TPS_ | 125.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 84.53 /s |
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
| _Avg. Confirmation Time (ms)_ | 86.4 |
| _P99_ | 88.1ms |
| _P95_ | 88.0ms |
| _P50_ | 86.5ms |
| _Tx validation time p50 (ms)_ | 31.3 |
| _End-to-end TPS_ | 1019.22 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.65 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.1 |
| _P99_ | 33.4ms |
| _P95_ | 30.8ms |
| _P50_ | 22.7ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 128.50 tx/s |
| _Sustained TPS_ | 124.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 87.09 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
