--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-05 16:23:18.24015236 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 873.98 | n/a | 33.4 | 34.1 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 196.75 | 194.87 | 5.0 | 6.4 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 985.44 | n/a | 29.5 | 30.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 165.28 | 164.46 | 6.0 | 7.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1059.55 | n/a | 27.5 | 28.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 173.53 | 173.09 | 5.7 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 848.41 | n/a | 69.0 | 70.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 132.36 | 131.38 | 14.9 | 20.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 748.02 | n/a | 78.4 | 79.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 91.71 | 91.73 | 21.5 | 26.1 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 755.74 | n/a | 77.2 | 79.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.92 | 96.19 | 19.8 | 25.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 626.94 | n/a | 138.9 | 143.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 106.02 | 106.63 | 27.6 | 37.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 571.44 | n/a | 152.7 | 156.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 82.08 | 80.53 | 36.2 | 44.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 566.88 | n/a | 155.5 | 158.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 90.74 | 87.14 | 32.7 | 40.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 34.1ms |
| _P95_ | 34.1ms |
| _P50_ | 33.6ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 873.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.27 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 9.0ms |
| _P95_ | 6.4ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 196.75 tx/s |
| _Sustained TPS_ | 194.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 196.75 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 30.4ms |
| _P95_ | 30.3ms |
| _P50_ | 29.7ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 985.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.70 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 7.4ms |
| _P95_ | 7.1ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 165.28 tx/s |
| _Sustained TPS_ | 164.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 165.28 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.5 |
| _P99_ | 28.1ms |
| _P95_ | 28.1ms |
| _P50_ | 27.7ms |
| _Tx validation time p50 (ms)_ | 9.1 |
| _End-to-end TPS_ | 1059.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.64 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.1ms |
| _P95_ | 6.7ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 173.53 tx/s |
| _Sustained TPS_ | 173.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 173.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 69.0 |
| _P99_ | 70.5ms |
| _P95_ | 70.5ms |
| _P50_ | 69.4ms |
| _Tx validation time p50 (ms)_ | 26.2 |
| _End-to-end TPS_ | 848.41 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.28 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 22.4ms |
| _P95_ | 20.9ms |
| _P50_ | 14.2ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 132.36 tx/s |
| _Sustained TPS_ | 131.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 132.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.4 |
| _P99_ | 79.6ms |
| _P95_ | 79.2ms |
| _P50_ | 78.9ms |
| _Tx validation time p50 (ms)_ | 29.3 |
| _End-to-end TPS_ | 748.02 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.93 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.5 |
| _P99_ | 27.3ms |
| _P95_ | 26.1ms |
| _P50_ | 21.7ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 91.71 tx/s |
| _Sustained TPS_ | 91.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 91.71 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.2 |
| _P99_ | 79.3ms |
| _P95_ | 79.2ms |
| _P50_ | 77.7ms |
| _Tx validation time p50 (ms)_ | 27.8 |
| _End-to-end TPS_ | 755.74 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.19 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.8 |
| _P99_ | 27.4ms |
| _P95_ | 25.5ms |
| _P50_ | 19.1ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 99.92 tx/s |
| _Sustained TPS_ | 96.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 138.9 |
| _P99_ | 143.3ms |
| _P95_ | 143.2ms |
| _P50_ | 140.0ms |
| _Tx validation time p50 (ms)_ | 50.8 |
| _End-to-end TPS_ | 626.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.93 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.6 |
| _P99_ | 43.0ms |
| _P95_ | 37.4ms |
| _P50_ | 26.7ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 106.02 tx/s |
| _Sustained TPS_ | 106.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 71.86 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 152.7 |
| _P99_ | 156.5ms |
| _P95_ | 156.2ms |
| _P50_ | 155.0ms |
| _Tx validation time p50 (ms)_ | 53.0 |
| _End-to-end TPS_ | 571.44 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.70 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.2 |
| _P99_ | 46.3ms |
| _P95_ | 44.4ms |
| _P50_ | 36.5ms |
| _Tx validation time p50 (ms)_ | 9.8 |
| _End-to-end TPS_ | 82.08 tx/s |
| _Sustained TPS_ | 80.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 54.72 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 155.5 |
| _P99_ | 158.1ms |
| _P95_ | 158.0ms |
| _P50_ | 157.2ms |
| _Tx validation time p50 (ms)_ | 50.6 |
| _End-to-end TPS_ | 566.88 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.60 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 42.2ms |
| _P95_ | 40.3ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 90.74 tx/s |
| _Sustained TPS_ | 87.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 61.50 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
