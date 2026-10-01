--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-01 08:37:35.694262493 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 926.07 | n/a | 31.7 | 32.1 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 177.90 | 177.84 | 5.5 | 6.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 997.76 | n/a | 29.4 | 29.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 147.03 | 146.44 | 6.7 | 9.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1147.35 | n/a | 25.5 | 25.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 173.75 | 173.26 | 5.7 | 6.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 934.91 | n/a | 62.7 | 63.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 143.39 | 143.75 | 13.7 | 16.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 853.97 | n/a | 68.4 | 69.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 110.81 | 107.95 | 17.8 | 23.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 694.40 | n/a | 84.7 | 86.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 113.14 | 108.60 | 17.5 | 25.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 718.85 | n/a | 123.1 | 124.6 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 117.32 | 114.75 | 25.0 | 33.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 627.85 | n/a | 140.9 | 142.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 96.23 | 95.74 | 30.5 | 36.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 668.56 | n/a | 131.6 | 132.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 96.83 | 96.18 | 30.5 | 42.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.7 |
| _P99_ | 32.2ms |
| _P95_ | 32.1ms |
| _P50_ | 31.9ms |
| _Tx validation time p50 (ms)_ | 17.6 |
| _End-to-end TPS_ | 926.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.74 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 141.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 9.1ms |
| _P95_ | 6.3ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 177.90 tx/s |
| _Sustained TPS_ | 177.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.90 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.4 |
| _P99_ | 29.9ms |
| _P95_ | 29.9ms |
| _P50_ | 29.7ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 997.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.7 |
| _P99_ | 12.1ms |
| _P95_ | 9.7ms |
| _P50_ | 6.3ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 147.03 tx/s |
| _Sustained TPS_ | 146.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 147.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.5 |
| _P99_ | 26.0ms |
| _P95_ | 25.9ms |
| _P50_ | 25.8ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 1147.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.49 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 6.4ms |
| _P95_ | 6.3ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 173.75 tx/s |
| _Sustained TPS_ | 173.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 173.75 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 62.7 |
| _P99_ | 64.0ms |
| _P95_ | 63.9ms |
| _P50_ | 63.0ms |
| _Tx validation time p50 (ms)_ | 20.6 |
| _End-to-end TPS_ | 934.91 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.7 |
| _P99_ | 19.3ms |
| _P95_ | 16.9ms |
| _P50_ | 13.4ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 143.39 tx/s |
| _Sustained TPS_ | 143.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 143.39 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 68.4 |
| _P99_ | 69.2ms |
| _P95_ | 69.2ms |
| _P50_ | 68.6ms |
| _Tx validation time p50 (ms)_ | 19.9 |
| _End-to-end TPS_ | 853.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.8 |
| _P99_ | 24.6ms |
| _P95_ | 23.2ms |
| _P50_ | 17.3ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 110.81 tx/s |
| _Sustained TPS_ | 107.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 110.81 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 148.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.7 |
| _P99_ | 86.2ms |
| _P95_ | 86.2ms |
| _P50_ | 84.7ms |
| _Tx validation time p50 (ms)_ | 39.2 |
| _End-to-end TPS_ | 694.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.15 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 26.3ms |
| _P95_ | 25.5ms |
| _P50_ | 17.0ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 113.14 tx/s |
| _Sustained TPS_ | 108.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 123.1 |
| _P99_ | 124.6ms |
| _P95_ | 124.6ms |
| _P50_ | 123.6ms |
| _Tx validation time p50 (ms)_ | 46.2 |
| _End-to-end TPS_ | 718.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.97 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 35.1ms |
| _P95_ | 33.7ms |
| _P50_ | 24.3ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 117.32 tx/s |
| _Sustained TPS_ | 114.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 80.82 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.9 |
| _P99_ | 142.7ms |
| _P95_ | 142.6ms |
| _P50_ | 140.9ms |
| _Tx validation time p50 (ms)_ | 41.1 |
| _End-to-end TPS_ | 627.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.95 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 30.5 |
| _P99_ | 39.9ms |
| _P95_ | 36.9ms |
| _P50_ | 31.0ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 96.23 tx/s |
| _Sustained TPS_ | 95.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 65.22 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.6 |
| _P99_ | 132.7ms |
| _P95_ | 132.6ms |
| _P50_ | 132.3ms |
| _Tx validation time p50 (ms)_ | 40.7 |
| _End-to-end TPS_ | 668.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.86 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 30.5 |
| _P99_ | 51.1ms |
| _P95_ | 42.7ms |
| _P50_ | 29.3ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 96.83 tx/s |
| _Sustained TPS_ | 96.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 65.63 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      
