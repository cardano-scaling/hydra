--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-27 14:57:57.028581958 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 898.76 | n/a | 32.7 | 33.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 207.72 | 207.06 | 4.7 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 890.21 | n/a | 33.0 | 33.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 161.61 | 161.86 | 6.1 | 7.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 879.41 | n/a | 33.4 | 33.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 183.99 | 185.19 | 5.4 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 874.73 | n/a | 66.9 | 67.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 125.60 | 124.15 | 15.7 | 19.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 639.33 | n/a | 92.4 | 93.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 86.57 | 86.39 | 22.8 | 28.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 792.50 | n/a | 74.3 | 74.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.7 | 92.11 | 89.12 | 21.5 | 26.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 601.12 | n/a | 147.2 | 149.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 100.39 | 103.41 | 29.3 | 38.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 597.35 | n/a | 147.9 | 149.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.3 | 71.56 | 71.86 | 41.2 | 49.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 642.54 | n/a | 137.1 | 139.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.2 | 77.84 | 75.02 | 37.9 | 48.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 33.3ms |
| _P95_ | 33.3ms |
| _P50_ | 33.0ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 898.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 133.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.7 |
| _P99_ | 6.3ms |
| _P95_ | 5.7ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 207.72 tx/s |
| _Sustained TPS_ | 207.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 207.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 33.5ms |
| _P95_ | 33.5ms |
| _P50_ | 33.3ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 890.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.35 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 8.0ms |
| _P95_ | 7.6ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 161.61 tx/s |
| _Sustained TPS_ | 161.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 161.61 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 33.9ms |
| _P95_ | 33.9ms |
| _P50_ | 33.7ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 879.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.63 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 126.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 6.9ms |
| _P95_ | 6.5ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 183.99 tx/s |
| _Sustained TPS_ | 185.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 68.1ms |
| _P95_ | 67.8ms |
| _P50_ | 67.2ms |
| _Tx validation time p50 (ms)_ | 22.5 |
| _End-to-end TPS_ | 874.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.7 |
| _P99_ | 24.1ms |
| _P95_ | 19.5ms |
| _P50_ | 15.7ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 125.60 tx/s |
| _Sustained TPS_ | 124.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 125.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 92.4 |
| _P99_ | 93.2ms |
| _P95_ | 93.1ms |
| _P50_ | 92.8ms |
| _Tx validation time p50 (ms)_ | 34.9 |
| _End-to-end TPS_ | 639.33 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.8 |
| _P99_ | 31.2ms |
| _P95_ | 28.5ms |
| _P50_ | 23.3ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 86.57 tx/s |
| _Sustained TPS_ | 86.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 86.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.3 |
| _P99_ | 74.9ms |
| _P95_ | 74.9ms |
| _P50_ | 74.6ms |
| _Tx validation time p50 (ms)_ | 22.7 |
| _End-to-end TPS_ | 792.50 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.42 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.5 |
| _P99_ | 29.5ms |
| _P95_ | 26.2ms |
| _P50_ | 21.9ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 92.11 tx/s |
| _Sustained TPS_ | 89.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.2 |
| _P99_ | 149.5ms |
| _P95_ | 149.4ms |
| _P50_ | 147.2ms |
| _Tx validation time p50 (ms)_ | 56.6 |
| _End-to-end TPS_ | 601.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.36 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.3 |
| _P99_ | 46.0ms |
| _P95_ | 38.8ms |
| _P50_ | 28.9ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 100.39 tx/s |
| _Sustained TPS_ | 103.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 68.04 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.9 |
| _P99_ | 149.5ms |
| _P95_ | 149.3ms |
| _P50_ | 148.9ms |
| _Tx validation time p50 (ms)_ | 40.7 |
| _End-to-end TPS_ | 597.35 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.27 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.2 |
| _P99_ | 54.3ms |
| _P95_ | 49.9ms |
| _P50_ | 41.4ms |
| _Tx validation time p50 (ms)_ | 11.8 |
| _End-to-end TPS_ | 71.56 tx/s |
| _Sustained TPS_ | 71.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 48.50 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 137.1 |
| _P99_ | 139.9ms |
| _P95_ | 139.8ms |
| _P50_ | 138.5ms |
| _Tx validation time p50 (ms)_ | 51.4 |
| _End-to-end TPS_ | 642.54 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.28 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 37.9 |
| _P99_ | 53.7ms |
| _P95_ | 48.9ms |
| _P50_ | 38.0ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 77.84 tx/s |
| _Sustained TPS_ | 75.02 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 52.76 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
