--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-26 10:50:02.770392599 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1160.21 | n/a | 25.2 | 25.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 199.58 | 195.40 | 4.9 | 7.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1090.90 | n/a | 26.7 | 27.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 167.55 | 164.75 | 5.9 | 7.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1082.05 | n/a | 27.0 | 27.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 187.46 | 185.78 | 5.3 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1065.44 | n/a | 54.6 | 56.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 149.06 | 148.02 | 13.3 | 15.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 965.26 | n/a | 59.8 | 61.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 106.89 | 104.93 | 18.5 | 22.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 999.07 | n/a | 58.7 | 59.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 112.40 | 105.61 | 17.6 | 24.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 759.95 | n/a | 115.1 | 118.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 111.87 | 111.54 | 26.4 | 36.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 626.71 | n/a | 139.6 | 142.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 87.67 | 87.45 | 33.6 | 41.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 713.66 | n/a | 123.5 | 124.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 94.92 | 92.25 | 31.4 | 41.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 25.6ms |
| _P95_ | 25.6ms |
| _P50_ | 25.3ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 1160.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.35 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 10.5ms |
| _P95_ | 7.0ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 199.58 tx/s |
| _Sustained TPS_ | 195.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 199.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.7 |
| _P99_ | 27.3ms |
| _P95_ | 27.2ms |
| _P50_ | 26.9ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 1090.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 72.73 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 11.5ms |
| _P95_ | 7.7ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 167.55 tx/s |
| _Sustained TPS_ | 164.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 167.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.0 |
| _P99_ | 27.5ms |
| _P95_ | 27.4ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 13.0 |
| _End-to-end TPS_ | 1082.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 72.14 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 6.5ms |
| _P95_ | 6.5ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 187.46 tx/s |
| _Sustained TPS_ | 185.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 187.46 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 54.6 |
| _P99_ | 56.1ms |
| _P95_ | 56.0ms |
| _P50_ | 54.8ms |
| _Tx validation time p50 (ms)_ | 23.2 |
| _End-to-end TPS_ | 1065.44 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 35.51 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.3 |
| _P99_ | 16.9ms |
| _P95_ | 15.6ms |
| _P50_ | 13.2ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 149.06 tx/s |
| _Sustained TPS_ | 148.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 149.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 59.8 |
| _P99_ | 61.6ms |
| _P95_ | 61.2ms |
| _P50_ | 60.4ms |
| _Tx validation time p50 (ms)_ | 21.1 |
| _End-to-end TPS_ | 965.26 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.18 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.5 |
| _P99_ | 23.9ms |
| _P95_ | 22.8ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 106.89 tx/s |
| _Sustained TPS_ | 104.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.89 /s |
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
| _Avg. Confirmation Time (ms)_ | 58.7 |
| _P99_ | 59.3ms |
| _P95_ | 59.2ms |
| _P50_ | 58.9ms |
| _Tx validation time p50 (ms)_ | 24.5 |
| _End-to-end TPS_ | 999.07 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.30 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.6 |
| _P99_ | 27.1ms |
| _P95_ | 24.9ms |
| _P50_ | 17.2ms |
| _Tx validation time p50 (ms)_ | 5.0 |
| _End-to-end TPS_ | 112.40 tx/s |
| _Sustained TPS_ | 105.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 112.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 115.1 |
| _P99_ | 118.1ms |
| _P95_ | 118.0ms |
| _P50_ | 114.8ms |
| _Tx validation time p50 (ms)_ | 52.0 |
| _End-to-end TPS_ | 759.95 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.89 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.4 |
| _P99_ | 46.2ms |
| _P95_ | 36.1ms |
| _P50_ | 25.1ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 111.87 tx/s |
| _Sustained TPS_ | 111.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 75.83 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 139.6 |
| _P99_ | 142.1ms |
| _P95_ | 142.0ms |
| _P50_ | 141.6ms |
| _Tx validation time p50 (ms)_ | 45.9 |
| _End-to-end TPS_ | 626.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.93 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.6 |
| _P99_ | 48.6ms |
| _P95_ | 41.7ms |
| _P50_ | 33.5ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 87.67 tx/s |
| _Sustained TPS_ | 87.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.42 /s |
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
| _Avg. Confirmation Time (ms)_ | 123.5 |
| _P99_ | 124.8ms |
| _P95_ | 124.6ms |
| _P50_ | 124.2ms |
| _Tx validation time p50 (ms)_ | 43.5 |
| _End-to-end TPS_ | 713.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.86 /s |
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
| _Avg. Confirmation Time (ms)_ | 31.4 |
| _P99_ | 43.5ms |
| _P95_ | 41.5ms |
| _P50_ | 30.6ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 94.92 tx/s |
| _Sustained TPS_ | 92.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.34 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
