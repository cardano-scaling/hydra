--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-02 06:46:39.754963852 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1163.22 | n/a | 25.2 | 25.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 234.49 | 231.73 | 4.2 | 5.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1064.54 | n/a | 27.5 | 28.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 192.79 | 195.65 | 5.1 | 7.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1078.23 | n/a | 27.1 | 27.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 203.84 | 199.84 | 4.8 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1039.87 | n/a | 56.2 | 56.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 166.36 | 163.33 | 11.8 | 17.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1068.79 | n/a | 54.2 | 55.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 128.22 | 126.63 | 15.4 | 20.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1177.29 | n/a | 49.3 | 50.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 125.05 | 121.52 | 15.8 | 22.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 977.90 | n/a | 89.6 | 91.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 131.14 | 123.15 | 22.7 | 45.7 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 991.03 | n/a | 88.6 | 90.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 118.60 | 116.80 | 25.0 | 35.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 925.45 | n/a | 84.6 | 94.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 103.06 | 98.69 | 28.6 | 51.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 25.6ms |
| _P95_ | 25.6ms |
| _P50_ | 25.3ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 1163.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.55 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.2 |
| _P99_ | 7.5ms |
| _P95_ | 5.0ms |
| _P50_ | 4.0ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 234.49 tx/s |
| _Sustained TPS_ | 231.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 234.49 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.5 |
| _P99_ | 28.0ms |
| _P95_ | 28.0ms |
| _P50_ | 27.7ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 1064.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.97 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.3ms |
| _P95_ | 7.0ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 192.79 tx/s |
| _Sustained TPS_ | 195.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.79 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.1 |
| _P99_ | 27.7ms |
| _P95_ | 27.6ms |
| _P50_ | 27.4ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 1078.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.88 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 7.5ms |
| _P95_ | 6.7ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 203.84 tx/s |
| _Sustained TPS_ | 199.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 203.84 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 56.2 |
| _P99_ | 57.0ms |
| _P95_ | 56.9ms |
| _P50_ | 56.5ms |
| _Tx validation time p50 (ms)_ | 22.6 |
| _End-to-end TPS_ | 1039.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 34.66 /s |
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
| _Avg. Confirmation Time (ms)_ | 11.8 |
| _P99_ | 18.9ms |
| _P95_ | 17.7ms |
| _P50_ | 11.0ms |
| _Tx validation time p50 (ms)_ | 2.9 |
| _End-to-end TPS_ | 166.36 tx/s |
| _Sustained TPS_ | 163.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 166.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 54.2 |
| _P99_ | 56.0ms |
| _P95_ | 55.9ms |
| _P50_ | 54.9ms |
| _Tx validation time p50 (ms)_ | 18.1 |
| _End-to-end TPS_ | 1068.79 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 35.63 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.4 |
| _P99_ | 21.6ms |
| _P95_ | 20.4ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 128.22 tx/s |
| _Sustained TPS_ | 126.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 128.22 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 140.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 49.3 |
| _P99_ | 50.8ms |
| _P95_ | 50.7ms |
| _P50_ | 49.6ms |
| _Tx validation time p50 (ms)_ | 16.4 |
| _End-to-end TPS_ | 1177.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.24 /s |
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
| _Avg. Confirmation Time (ms)_ | 15.8 |
| _P99_ | 24.1ms |
| _P95_ | 22.3ms |
| _P50_ | 15.6ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 125.05 tx/s |
| _Sustained TPS_ | 121.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 125.05 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 89.6 |
| _P99_ | 91.4ms |
| _P95_ | 91.3ms |
| _P50_ | 90.7ms |
| _Tx validation time p50 (ms)_ | 32.9 |
| _End-to-end TPS_ | 977.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.73 /s |
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
| _Avg. Confirmation Time (ms)_ | 22.7 |
| _P99_ | 88.7ms |
| _P95_ | 45.7ms |
| _P50_ | 19.4ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 131.14 tx/s |
| _Sustained TPS_ | 123.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 87.43 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 88.6 |
| _P99_ | 90.1ms |
| _P95_ | 90.0ms |
| _P50_ | 89.5ms |
| _Tx validation time p50 (ms)_ | 29.4 |
| _End-to-end TPS_ | 991.03 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.02 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 38.0ms |
| _P95_ | 35.4ms |
| _P50_ | 24.9ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 118.60 tx/s |
| _Sustained TPS_ | 116.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 80.38 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.6 |
| _P99_ | 94.9ms |
| _P95_ | 94.8ms |
| _P50_ | 80.0ms |
| _Tx validation time p50 (ms)_ | 39.9 |
| _End-to-end TPS_ | 925.45 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.57 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.6 |
| _P99_ | 53.1ms |
| _P95_ | 51.4ms |
| _P50_ | 26.7ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 103.06 tx/s |
| _Sustained TPS_ | 98.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 70.99 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
