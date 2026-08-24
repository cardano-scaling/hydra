--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-24 08:46:59.323453751 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1330.47 | n/a | 21.8 | 22.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 215.35 | 213.39 | 4.6 | 6.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 868.30 | n/a | 33.7 | 34.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 170.70 | 169.41 | 5.8 | 6.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1169.27 | n/a | 24.9 | 25.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 183.30 | 177.89 | 5.4 | 6.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1159.73 | n/a | 50.3 | 51.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 136.37 | 133.83 | 14.5 | 20.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 858.16 | n/a | 68.0 | 69.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 101.04 | 99.63 | 19.5 | 25.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1002.37 | n/a | 58.1 | 59.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 104.29 | 98.74 | 19.0 | 25.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 689.74 | n/a | 126.0 | 129.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 107.54 | 109.72 | 27.0 | 34.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 616.23 | n/a | 141.3 | 145.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 80.45 | 80.36 | 36.0 | 45.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 789.91 | n/a | 110.4 | 113.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.68 | 82.93 | 34.4 | 44.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 21.8 |
| _P99_ | 22.3ms |
| _P95_ | 22.3ms |
| _P50_ | 22.0ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 1330.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 88.70 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 6.1ms |
| _P95_ | 6.1ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 215.35 tx/s |
| _Sustained TPS_ | 213.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 215.35 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 33.7 |
| _P99_ | 34.3ms |
| _P95_ | 34.3ms |
| _P50_ | 33.9ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 868.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.89 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.9ms |
| _P95_ | 6.8ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 170.70 tx/s |
| _Sustained TPS_ | 169.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 170.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 24.9 |
| _P99_ | 25.4ms |
| _P95_ | 25.4ms |
| _P50_ | 25.1ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 1169.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.95 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 10.6ms |
| _P95_ | 6.6ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 183.30 tx/s |
| _Sustained TPS_ | 177.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.30 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 50.3 |
| _P99_ | 51.0ms |
| _P95_ | 51.0ms |
| _P50_ | 50.4ms |
| _Tx validation time p50 (ms)_ | 23.4 |
| _End-to-end TPS_ | 1159.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 38.66 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 14.5 |
| _P99_ | 23.7ms |
| _P95_ | 20.0ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 136.37 tx/s |
| _Sustained TPS_ | 133.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 136.37 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 68.0 |
| _P99_ | 69.6ms |
| _P95_ | 69.5ms |
| _P50_ | 68.6ms |
| _Tx validation time p50 (ms)_ | 22.8 |
| _End-to-end TPS_ | 858.16 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.61 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.5 |
| _P99_ | 27.7ms |
| _P95_ | 25.7ms |
| _P50_ | 19.3ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 101.04 tx/s |
| _Sustained TPS_ | 99.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.04 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 58.1 |
| _P99_ | 59.6ms |
| _P95_ | 59.5ms |
| _P50_ | 58.4ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 1002.37 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.41 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 29.3ms |
| _P95_ | 25.7ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 104.29 tx/s |
| _Sustained TPS_ | 98.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 126.0 |
| _P99_ | 129.4ms |
| _P95_ | 129.0ms |
| _P50_ | 127.6ms |
| _Tx validation time p50 (ms)_ | 57.3 |
| _End-to-end TPS_ | 689.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.33 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 27.0 |
| _P99_ | 37.9ms |
| _P95_ | 34.2ms |
| _P50_ | 26.4ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 107.54 tx/s |
| _Sustained TPS_ | 109.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 74.08 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 141.3 |
| _P99_ | 145.1ms |
| _P95_ | 145.0ms |
| _P50_ | 142.4ms |
| _Tx validation time p50 (ms)_ | 59.1 |
| _End-to-end TPS_ | 616.23 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.69 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 36.0 |
| _P99_ | 56.2ms |
| _P95_ | 45.9ms |
| _P50_ | 35.9ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 80.45 tx/s |
| _Sustained TPS_ | 80.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 55.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 110.4 |
| _P99_ | 113.7ms |
| _P95_ | 113.6ms |
| _P50_ | 111.4ms |
| _Tx validation time p50 (ms)_ | 50.2 |
| _End-to-end TPS_ | 789.91 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.55 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 34.4 |
| _P99_ | 50.5ms |
| _P95_ | 44.8ms |
| _P50_ | 34.2ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 85.68 tx/s |
| _Sustained TPS_ | 82.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 59.02 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
