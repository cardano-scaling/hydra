--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-21 11:35:39.724514525 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1534.60 | n/a | 19.0 | 19.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 254.71 | 251.39 | 3.9 | 4.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1410.27 | n/a | 20.6 | 21.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 204.95 | 203.74 | 4.8 | 6.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1170.70 | n/a | 25.1 | 25.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 213.55 | 209.70 | 4.6 | 6.1 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1209.34 | n/a | 48.3 | 49.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 166.91 | 166.33 | 11.9 | 17.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 959.04 | n/a | 60.7 | 62.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 123.31 | 127.49 | 15.9 | 23.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 676.27 | n/a | 87.5 | 88.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 129.40 | 124.31 | 15.3 | 21.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1011.32 | n/a | 86.2 | 88.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 131.61 | 133.96 | 22.4 | 30.3 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 783.90 | n/a | 111.5 | 113.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 98.87 | 99.07 | 29.8 | 37.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 743.09 | n/a | 118.5 | 120.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.4 | 62.71 | 55.54 | 47.7 | 173.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 19.3ms |
| _P95_ | 19.3ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 1534.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 102.31 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 3.9 |
| _P99_ | 6.2ms |
| _P95_ | 4.7ms |
| _P50_ | 3.6ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 254.71 tx/s |
| _Sustained TPS_ | 251.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 254.71 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 20.6 |
| _P99_ | 21.1ms |
| _P95_ | 21.0ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 1410.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 94.02 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 6.6ms |
| _P95_ | 6.3ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 204.95 tx/s |
| _Sustained TPS_ | 203.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 204.95 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 25.1 |
| _P99_ | 25.4ms |
| _P95_ | 25.4ms |
| _P50_ | 25.2ms |
| _Tx validation time p50 (ms)_ | 13.7 |
| _End-to-end TPS_ | 1170.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 78.05 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 9.2ms |
| _P95_ | 6.1ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 213.55 tx/s |
| _Sustained TPS_ | 209.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 213.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 48.3 |
| _P99_ | 49.4ms |
| _P95_ | 49.3ms |
| _P50_ | 48.4ms |
| _Tx validation time p50 (ms)_ | 20.9 |
| _End-to-end TPS_ | 1209.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 40.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 11.9 |
| _P99_ | 26.3ms |
| _P95_ | 17.6ms |
| _P50_ | 10.6ms |
| _Tx validation time p50 (ms)_ | 2.9 |
| _End-to-end TPS_ | 166.91 tx/s |
| _Sustained TPS_ | 166.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 166.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 60.7 |
| _P99_ | 62.4ms |
| _P95_ | 62.3ms |
| _P50_ | 60.9ms |
| _Tx validation time p50 (ms)_ | 18.3 |
| _End-to-end TPS_ | 959.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.97 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 15.9 |
| _P99_ | 28.9ms |
| _P95_ | 23.3ms |
| _P50_ | 15.4ms |
| _Tx validation time p50 (ms)_ | 4.8 |
| _End-to-end TPS_ | 123.31 tx/s |
| _Sustained TPS_ | 127.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 123.31 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 87.5 |
| _P99_ | 88.0ms |
| _P95_ | 88.0ms |
| _P50_ | 87.6ms |
| _Tx validation time p50 (ms)_ | 26.3 |
| _End-to-end TPS_ | 676.27 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.54 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 15.3 |
| _P99_ | 23.0ms |
| _P95_ | 21.6ms |
| _P50_ | 14.6ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 129.40 tx/s |
| _Sustained TPS_ | 124.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 129.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 86.2 |
| _P99_ | 88.2ms |
| _P95_ | 88.1ms |
| _P50_ | 87.1ms |
| _Tx validation time p50 (ms)_ | 51.1 |
| _End-to-end TPS_ | 1011.32 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.47 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 22.4 |
| _P99_ | 32.3ms |
| _P95_ | 30.3ms |
| _P50_ | 22.1ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 131.61 tx/s |
| _Sustained TPS_ | 133.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 89.21 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 111.5 |
| _P99_ | 113.3ms |
| _P95_ | 113.2ms |
| _P50_ | 111.7ms |
| _Tx validation time p50 (ms)_ | 54.5 |
| _End-to-end TPS_ | 783.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 29.8 |
| _P99_ | 40.3ms |
| _P95_ | 37.9ms |
| _P50_ | 29.1ms |
| _Tx validation time p50 (ms)_ | 8.4 |
| _End-to-end TPS_ | 98.87 tx/s |
| _Sustained TPS_ | 99.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 67.01 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 118.5 |
| _P99_ | 120.3ms |
| _P95_ | 120.3ms |
| _P50_ | 118.9ms |
| _Tx validation time p50 (ms)_ | 77.2 |
| _End-to-end TPS_ | 743.09 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.51 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 47.7 |
| _P99_ | 181.0ms |
| _P95_ | 173.0ms |
| _P50_ | 26.5ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 62.71 tx/s |
| _Sustained TPS_ | 55.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 41.81 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
