--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-01 18:18:49.223157239 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1238.85 | n/a | 23.6 | 24.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 219.71 | 221.54 | 4.5 | 5.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1101.89 | n/a | 26.4 | 27.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 167.27 | 165.56 | 5.9 | 7.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1165.24 | n/a | 25.0 | 25.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 179.12 | 173.41 | 5.5 | 7.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1067.26 | n/a | 54.5 | 55.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 151.62 | 151.19 | 13.0 | 15.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 920.30 | n/a | 63.9 | 65.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 106.50 | 105.00 | 18.3 | 22.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 989.63 | n/a | 59.0 | 60.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 113.68 | 108.05 | 17.4 | 21.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 723.72 | n/a | 119.2 | 122.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 113.42 | 113.64 | 25.8 | 34.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 683.81 | n/a | 128.9 | 130.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.74 | 87.21 | 33.6 | 39.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 691.94 | n/a | 126.1 | 127.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 84.65 | 81.87 | 35.0 | 46.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 23.6 |
| _P99_ | 24.0ms |
| _P95_ | 24.0ms |
| _P50_ | 23.7ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 1238.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 82.59 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 6.0ms |
| _P95_ | 5.1ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 219.71 tx/s |
| _Sustained TPS_ | 221.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 219.71 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.4 |
| _P99_ | 27.0ms |
| _P95_ | 27.0ms |
| _P50_ | 26.6ms |
| _Tx validation time p50 (ms)_ | 13.6 |
| _End-to-end TPS_ | 1101.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 73.46 /s |
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
| _P99_ | 7.7ms |
| _P95_ | 7.4ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 167.27 tx/s |
| _Sustained TPS_ | 165.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 167.27 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 25.5ms |
| _P95_ | 25.5ms |
| _P50_ | 25.2ms |
| _Tx validation time p50 (ms)_ | 15.7 |
| _End-to-end TPS_ | 1165.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.68 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 11.3ms |
| _P95_ | 7.9ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 179.12 tx/s |
| _Sustained TPS_ | 173.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 179.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 54.5 |
| _P99_ | 55.9ms |
| _P95_ | 55.9ms |
| _P50_ | 54.8ms |
| _Tx validation time p50 (ms)_ | 35.6 |
| _End-to-end TPS_ | 1067.26 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 35.58 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.0 |
| _P99_ | 22.6ms |
| _P95_ | 15.5ms |
| _P50_ | 12.4ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 151.62 tx/s |
| _Sustained TPS_ | 151.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 151.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.9 |
| _P99_ | 65.1ms |
| _P95_ | 65.0ms |
| _P50_ | 63.9ms |
| _Tx validation time p50 (ms)_ | 31.5 |
| _End-to-end TPS_ | 920.30 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.68 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.3 |
| _P99_ | 25.0ms |
| _P95_ | 22.4ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 106.50 tx/s |
| _Sustained TPS_ | 105.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.50 /s |
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
| _Avg. Confirmation Time (ms)_ | 59.0 |
| _P99_ | 60.4ms |
| _P95_ | 60.4ms |
| _P50_ | 59.2ms |
| _Tx validation time p50 (ms)_ | 32.1 |
| _End-to-end TPS_ | 989.63 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.99 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.4 |
| _P99_ | 24.0ms |
| _P95_ | 21.4ms |
| _P50_ | 17.4ms |
| _Tx validation time p50 (ms)_ | 4.7 |
| _End-to-end TPS_ | 113.68 tx/s |
| _Sustained TPS_ | 108.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.68 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 119.2 |
| _P99_ | 123.2ms |
| _P95_ | 122.7ms |
| _P50_ | 119.3ms |
| _Tx validation time p50 (ms)_ | 43.6 |
| _End-to-end TPS_ | 723.72 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.08 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.8 |
| _P99_ | 41.7ms |
| _P95_ | 34.7ms |
| _P50_ | 24.8ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 113.42 tx/s |
| _Sustained TPS_ | 113.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 78.13 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.9 |
| _P99_ | 130.8ms |
| _P95_ | 130.7ms |
| _P50_ | 128.8ms |
| _Tx validation time p50 (ms)_ | 48.5 |
| _End-to-end TPS_ | 683.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.20 /s |
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
| _P99_ | 47.1ms |
| _P95_ | 39.9ms |
| _P50_ | 33.3ms |
| _Tx validation time p50 (ms)_ | 10.4 |
| _End-to-end TPS_ | 87.74 tx/s |
| _Sustained TPS_ | 87.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.47 /s |
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
| _Avg. Confirmation Time (ms)_ | 126.1 |
| _P99_ | 128.2ms |
| _P95_ | 127.9ms |
| _P50_ | 126.8ms |
| _Tx validation time p50 (ms)_ | 44.8 |
| _End-to-end TPS_ | 691.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.38 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.0 |
| _P99_ | 50.4ms |
| _P95_ | 46.0ms |
| _P50_ | 34.4ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 84.65 tx/s |
| _Sustained TPS_ | 81.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.37 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
