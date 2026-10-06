--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-06 13:48:43.786480534 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1021.99 | n/a | 28.5 | 29.1 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 202.07 | 201.15 | 4.9 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 860.59 | n/a | 34.0 | 34.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 159.76 | 158.93 | 6.2 | 7.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 954.77 | n/a | 30.6 | 31.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 174.07 | 170.77 | 5.7 | 7.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 959.34 | n/a | 61.1 | 61.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 136.43 | 136.05 | 14.5 | 18.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 732.43 | n/a | 80.1 | 81.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 99.85 | 100.76 | 19.8 | 24.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 986.89 | n/a | 59.1 | 60.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 102.68 | 98.17 | 19.3 | 25.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 638.56 | n/a | 137.3 | 139.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 99.70 | 99.14 | 29.7 | 39.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 560.59 | n/a | 156.2 | 160.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 81.11 | 83.24 | 36.4 | 43.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 640.95 | n/a | 137.4 | 139.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.90 | 88.14 | 32.9 | 42.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.5 |
| _P99_ | 29.1ms |
| _P95_ | 29.1ms |
| _P50_ | 28.7ms |
| _Tx validation time p50 (ms)_ | 11.1 |
| _End-to-end TPS_ | 1021.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 68.13 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 6.1ms |
| _P95_ | 5.7ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 202.07 tx/s |
| _Sustained TPS_ | 201.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 202.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 34.6ms |
| _P95_ | 34.6ms |
| _P50_ | 34.3ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 860.59 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.37 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.2 |
| _P99_ | 8.1ms |
| _P95_ | 7.9ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 159.76 tx/s |
| _Sustained TPS_ | 158.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 159.76 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.6 |
| _P99_ | 31.2ms |
| _P95_ | 31.2ms |
| _P50_ | 30.8ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 954.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.65 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.7 |
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
| _P99_ | 7.9ms |
| _P95_ | 7.3ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 174.07 tx/s |
| _Sustained TPS_ | 170.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 174.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.1 |
| _P99_ | 61.8ms |
| _P95_ | 61.8ms |
| _P50_ | 61.3ms |
| _Tx validation time p50 (ms)_ | 26.4 |
| _End-to-end TPS_ | 959.34 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.98 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.5 |
| _P99_ | 20.2ms |
| _P95_ | 18.0ms |
| _P50_ | 14.1ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 136.43 tx/s |
| _Sustained TPS_ | 136.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 136.43 /s |
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
| _Avg. Confirmation Time (ms)_ | 80.1 |
| _P99_ | 81.4ms |
| _P95_ | 81.0ms |
| _P50_ | 80.5ms |
| _Tx validation time p50 (ms)_ | 26.3 |
| _End-to-end TPS_ | 732.43 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.41 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.8 |
| _P99_ | 26.8ms |
| _P95_ | 24.8ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 99.85 tx/s |
| _Sustained TPS_ | 100.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 59.1 |
| _P99_ | 60.7ms |
| _P95_ | 60.6ms |
| _P50_ | 59.5ms |
| _Tx validation time p50 (ms)_ | 27.8 |
| _End-to-end TPS_ | 986.89 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.90 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 28.3ms |
| _P95_ | 25.3ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 102.68 tx/s |
| _Sustained TPS_ | 98.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.68 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 137.3 |
| _P99_ | 139.6ms |
| _P95_ | 139.4ms |
| _P50_ | 138.7ms |
| _Tx validation time p50 (ms)_ | 40.1 |
| _End-to-end TPS_ | 638.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.19 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.7 |
| _P99_ | 42.6ms |
| _P95_ | 39.5ms |
| _P50_ | 28.5ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 99.70 tx/s |
| _Sustained TPS_ | 99.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 67.57 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 156.2 |
| _P99_ | 160.3ms |
| _P95_ | 160.2ms |
| _P50_ | 157.6ms |
| _Tx validation time p50 (ms)_ | 47.3 |
| _End-to-end TPS_ | 560.59 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.46 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.4 |
| _P99_ | 46.8ms |
| _P95_ | 43.5ms |
| _P50_ | 36.7ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 81.11 tx/s |
| _Sustained TPS_ | 83.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.98 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 137.4 |
| _P99_ | 139.7ms |
| _P95_ | 139.6ms |
| _P50_ | 137.2ms |
| _Tx validation time p50 (ms)_ | 46.0 |
| _End-to-end TPS_ | 640.95 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.24 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.9 |
| _P99_ | 44.5ms |
| _P95_ | 42.3ms |
| _P50_ | 32.3ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 88.90 tx/s |
| _Sustained TPS_ | 88.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 61.24 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
