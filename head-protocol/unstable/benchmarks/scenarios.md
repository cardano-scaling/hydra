--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-22 18:45:08.341139351 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 838.54 | n/a | 34.8 | 35.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 187.79 | 186.30 | 5.3 | 6.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 837.10 | n/a | 34.8 | 35.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 158.42 | 155.70 | 6.2 | 8.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 803.37 | n/a | 36.4 | 37.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 163.47 | 162.65 | 6.1 | 7.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 891.29 | n/a | 65.2 | 67.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 117.96 | 118.19 | 16.8 | 21.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 733.28 | n/a | 79.7 | 80.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 94.82 | 93.45 | 20.9 | 25.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 714.22 | n/a | 81.8 | 83.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 94.63 | 91.28 | 20.9 | 27.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 641.06 | n/a | 136.6 | 140.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 101.73 | 100.14 | 29.3 | 36.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 598.92 | n/a | 146.1 | 148.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.5 | 61.12 | 61.19 | 48.3 | 55.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 611.19 | n/a | 143.3 | 146.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 87.36 | 82.70 | 34.0 | 42.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 35.6ms |
| _P95_ | 35.6ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 14.5 |
| _End-to-end TPS_ | 838.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.90 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 9.3ms |
| _P95_ | 6.8ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 187.79 tx/s |
| _Sustained TPS_ | 186.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 187.79 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 35.7ms |
| _P95_ | 35.6ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 837.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.81 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.2 |
| _P99_ | 9.0ms |
| _P95_ | 8.1ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 158.42 tx/s |
| _Sustained TPS_ | 155.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 158.42 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.4 |
| _P99_ | 37.2ms |
| _P95_ | 37.1ms |
| _P50_ | 36.8ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 803.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.56 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 10.1ms |
| _P95_ | 7.7ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 163.47 tx/s |
| _Sustained TPS_ | 162.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 163.47 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.2 |
| _P99_ | 67.2ms |
| _P95_ | 67.1ms |
| _P50_ | 65.0ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 891.29 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.71 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.8 |
| _P99_ | 23.3ms |
| _P95_ | 21.8ms |
| _P50_ | 16.3ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 117.96 tx/s |
| _Sustained TPS_ | 118.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 117.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.7 |
| _P99_ | 81.0ms |
| _P95_ | 80.9ms |
| _P50_ | 80.0ms |
| _Tx validation time p50 (ms)_ | 29.8 |
| _End-to-end TPS_ | 733.28 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.44 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.9 |
| _P99_ | 27.1ms |
| _P95_ | 25.2ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 94.82 tx/s |
| _Sustained TPS_ | 93.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 94.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.8 |
| _P99_ | 83.8ms |
| _P95_ | 83.7ms |
| _P50_ | 82.2ms |
| _Tx validation time p50 (ms)_ | 27.8 |
| _End-to-end TPS_ | 714.22 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.9 |
| _P99_ | 29.0ms |
| _P95_ | 27.1ms |
| _P50_ | 20.7ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 94.63 tx/s |
| _Sustained TPS_ | 91.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 94.63 /s |
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
| _Avg. Confirmation Time (ms)_ | 136.6 |
| _P99_ | 140.2ms |
| _P95_ | 140.0ms |
| _P50_ | 137.8ms |
| _Tx validation time p50 (ms)_ | 52.8 |
| _End-to-end TPS_ | 641.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.25 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.3 |
| _P99_ | 37.8ms |
| _P95_ | 36.1ms |
| _P50_ | 29.5ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 101.73 tx/s |
| _Sustained TPS_ | 100.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 67.82 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.1 |
| _P99_ | 148.7ms |
| _P95_ | 148.6ms |
| _P50_ | 147.0ms |
| _Tx validation time p50 (ms)_ | 54.7 |
| _End-to-end TPS_ | 598.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.31 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 48.3 |
| _P99_ | 61.9ms |
| _P95_ | 55.9ms |
| _P50_ | 48.5ms |
| _Tx validation time p50 (ms)_ | 14.6 |
| _End-to-end TPS_ | 61.12 tx/s |
| _Sustained TPS_ | 61.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 41.42 /s |
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
| _Avg. Confirmation Time (ms)_ | 143.3 |
| _P99_ | 147.0ms |
| _P95_ | 146.9ms |
| _P50_ | 144.1ms |
| _Tx validation time p50 (ms)_ | 52.5 |
| _End-to-end TPS_ | 611.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.58 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.0 |
| _P99_ | 48.9ms |
| _P95_ | 42.9ms |
| _P50_ | 33.1ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 87.36 tx/s |
| _Sustained TPS_ | 82.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.21 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
