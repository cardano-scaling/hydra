--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-22 15:55:05.775770563 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 877.48 | n/a | 33.4 | 34.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 180.86 | 184.25 | 5.5 | 6.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 790.63 | n/a | 36.9 | 37.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 157.67 | 160.08 | 6.3 | 8.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 838.83 | n/a | 34.8 | 35.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 163.82 | 160.60 | 6.0 | 7.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 863.05 | n/a | 66.9 | 68.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 118.32 | 119.32 | 16.7 | 21.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 680.44 | n/a | 86.7 | 87.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 93.10 | 92.26 | 21.2 | 26.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 767.77 | n/a | 76.2 | 77.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 96.84 | 92.62 | 20.5 | 27.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 571.14 | n/a | 153.7 | 156.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 97.29 | 96.60 | 30.0 | 43.4 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 601.07 | n/a | 145.8 | 149.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.53 | 82.61 | 35.6 | 44.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 622.61 | n/a | 139.9 | 143.6 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 82.84 | 82.78 | 35.6 | 45.7 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.4 |
| _P99_ | 34.0ms |
| _P95_ | 34.0ms |
| _P50_ | 33.7ms |
| _Tx validation time p50 (ms)_ | 12.4 |
| _End-to-end TPS_ | 877.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 58.50 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 133.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 7.5ms |
| _P95_ | 6.9ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 180.86 tx/s |
| _Sustained TPS_ | 184.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 180.86 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.9 |
| _P99_ | 37.8ms |
| _P95_ | 37.7ms |
| _P50_ | 37.3ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 790.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 52.71 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.3 |
| _P99_ | 9.7ms |
| _P95_ | 8.1ms |
| _P50_ | 6.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 157.67 tx/s |
| _Sustained TPS_ | 160.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 157.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 35.6ms |
| _P95_ | 35.5ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 14.2 |
| _End-to-end TPS_ | 838.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 8.9ms |
| _P95_ | 7.2ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 163.82 tx/s |
| _Sustained TPS_ | 160.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 163.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 69.0ms |
| _P95_ | 68.6ms |
| _P50_ | 67.7ms |
| _Tx validation time p50 (ms)_ | 38.4 |
| _End-to-end TPS_ | 863.05 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.7 |
| _P99_ | 21.8ms |
| _P95_ | 21.3ms |
| _P50_ | 16.6ms |
| _Tx validation time p50 (ms)_ | 4.4 |
| _End-to-end TPS_ | 118.32 tx/s |
| _Sustained TPS_ | 119.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 118.32 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.7 |
| _P99_ | 88.0ms |
| _P95_ | 87.9ms |
| _P50_ | 87.2ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 680.44 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.68 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.2 |
| _P99_ | 29.3ms |
| _P95_ | 26.4ms |
| _P50_ | 21.0ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 93.10 tx/s |
| _Sustained TPS_ | 92.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 93.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 76.2 |
| _P99_ | 77.3ms |
| _P95_ | 77.2ms |
| _P50_ | 76.5ms |
| _Tx validation time p50 (ms)_ | 26.4 |
| _End-to-end TPS_ | 767.77 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.59 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.5 |
| _P99_ | 35.8ms |
| _P95_ | 27.9ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 96.84 tx/s |
| _Sustained TPS_ | 92.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 153.7 |
| _P99_ | 156.6ms |
| _P95_ | 156.4ms |
| _P50_ | 155.9ms |
| _Tx validation time p50 (ms)_ | 40.5 |
| _End-to-end TPS_ | 571.14 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.69 /s |
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
| _Avg. Confirmation Time (ms)_ | 30.0 |
| _P99_ | 45.9ms |
| _P95_ | 43.4ms |
| _P50_ | 28.8ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 97.29 tx/s |
| _Sustained TPS_ | 96.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 67.02 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.8 |
| _P99_ | 149.5ms |
| _P95_ | 149.4ms |
| _P50_ | 146.6ms |
| _Tx validation time p50 (ms)_ | 51.6 |
| _End-to-end TPS_ | 601.07 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.36 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 50.1ms |
| _P95_ | 44.8ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 10.7 |
| _End-to-end TPS_ | 83.53 tx/s |
| _Sustained TPS_ | 82.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 55.69 /s |
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
| _Avg. Confirmation Time (ms)_ | 139.9 |
| _P99_ | 143.7ms |
| _P95_ | 143.6ms |
| _P50_ | 140.6ms |
| _Tx validation time p50 (ms)_ | 44.3 |
| _End-to-end TPS_ | 622.61 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.84 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 50.8ms |
| _P95_ | 45.7ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 82.84 tx/s |
| _Sustained TPS_ | 82.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.15 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
