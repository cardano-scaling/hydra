--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-24 14:34:04.256778533 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1295.71 | n/a | 22.5 | 22.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 199.34 | 195.13 | 4.9 | 7.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 867.59 | n/a | 34.0 | 34.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 167.34 | 173.37 | 5.9 | 7.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 839.06 | n/a | 35.1 | 35.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 188.67 | 185.63 | 5.2 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 918.51 | n/a | 63.8 | 65.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 135.75 | 130.79 | 14.6 | 21.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 816.03 | n/a | 70.8 | 72.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 92.81 | 89.53 | 21.3 | 28.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 916.62 | n/a | 61.9 | 63.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.63 | 93.57 | 19.9 | 29.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 829.14 | n/a | 105.8 | 106.7 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 106.88 | 108.29 | 27.4 | 38.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 593.36 | n/a | 148.7 | 151.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 78.02 | 78.78 | 37.7 | 49.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 693.92 | n/a | 126.4 | 129.4 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.18 | 83.66 | 34.9 | 45.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 22.5 |
| _P99_ | 22.9ms |
| _P95_ | 22.9ms |
| _P50_ | 22.6ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 1295.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 86.38 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 10.3ms |
| _P95_ | 7.3ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 199.34 tx/s |
| _Sustained TPS_ | 195.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 199.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 34.5ms |
| _P95_ | 34.5ms |
| _P50_ | 34.2ms |
| _Tx validation time p50 (ms)_ | 24.7 |
| _End-to-end TPS_ | 867.59 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.84 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 11.1ms |
| _P95_ | 7.3ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 167.34 tx/s |
| _Sustained TPS_ | 173.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 167.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 35.5ms |
| _P95_ | 35.5ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 12.2 |
| _End-to-end TPS_ | 839.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.94 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 6.6ms |
| _P95_ | 6.5ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 188.67 tx/s |
| _Sustained TPS_ | 185.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 188.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.8 |
| _P99_ | 65.0ms |
| _P95_ | 65.0ms |
| _P50_ | 64.1ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 918.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.62 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.6 |
| _P99_ | 25.6ms |
| _P95_ | 21.6ms |
| _P50_ | 13.5ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 135.75 tx/s |
| _Sustained TPS_ | 130.79 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 135.75 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.8 |
| _P99_ | 72.2ms |
| _P95_ | 72.1ms |
| _P50_ | 71.0ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 816.03 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.20 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.3 |
| _P99_ | 30.9ms |
| _P95_ | 28.5ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 92.81 tx/s |
| _Sustained TPS_ | 89.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.81 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.9 |
| _P99_ | 63.4ms |
| _P95_ | 63.3ms |
| _P50_ | 62.1ms |
| _Tx validation time p50 (ms)_ | 24.8 |
| _End-to-end TPS_ | 916.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.55 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.9 |
| _P99_ | 32.8ms |
| _P95_ | 29.6ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 99.63 tx/s |
| _Sustained TPS_ | 93.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 105.8 |
| _P99_ | 106.9ms |
| _P95_ | 106.7ms |
| _P50_ | 105.9ms |
| _Tx validation time p50 (ms)_ | 63.6 |
| _End-to-end TPS_ | 829.14 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 18.43 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.4 |
| _P99_ | 42.5ms |
| _P95_ | 38.2ms |
| _P50_ | 25.8ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 106.88 tx/s |
| _Sustained TPS_ | 108.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 73.63 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 148.7 |
| _P99_ | 151.5ms |
| _P95_ | 151.4ms |
| _P50_ | 148.7ms |
| _Tx validation time p50 (ms)_ | 71.0 |
| _End-to-end TPS_ | 593.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.19 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.7 |
| _P99_ | 57.6ms |
| _P95_ | 49.0ms |
| _P50_ | 37.4ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 78.02 tx/s |
| _Sustained TPS_ | 78.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 53.75 /s |
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
| _Avg. Confirmation Time (ms)_ | 126.4 |
| _P99_ | 129.5ms |
| _P95_ | 129.4ms |
| _P50_ | 126.2ms |
| _Tx validation time p50 (ms)_ | 52.3 |
| _End-to-end TPS_ | 693.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 49.2ms |
| _P95_ | 45.6ms |
| _P50_ | 34.1ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 85.18 tx/s |
| _Sustained TPS_ | 83.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.73 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
