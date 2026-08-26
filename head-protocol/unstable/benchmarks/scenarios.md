--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-26 18:20:01.136939106 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1176.97 | n/a | 24.8 | 25.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 213.68 | 214.06 | 4.6 | 6.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1122.25 | n/a | 26.0 | 26.4 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 130.58 | 134.94 | 7.6 | 12.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1161.65 | n/a | 24.9 | 25.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 194.06 | 190.72 | 5.1 | 5.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 1107.83 | n/a | 52.4 | 53.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 151.40 | 151.24 | 12.9 | 16.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 936.49 | n/a | 62.3 | 62.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 88.45 | 101.80 | 22.3 | 25.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1002.72 | n/a | 58.0 | 59.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 111.90 | 106.10 | 17.7 | 22.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 734.88 | n/a | 119.8 | 122.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 118.72 | 116.36 | 25.0 | 32.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 711.94 | n/a | 122.8 | 125.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 89.92 | 91.94 | 32.4 | 41.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 706.45 | n/a | 124.5 | 126.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 94.92 | 92.44 | 31.2 | 40.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.8 |
| _P99_ | 25.3ms |
| _P95_ | 25.2ms |
| _P50_ | 25.0ms |
| _Tx validation time p50 (ms)_ | 12.3 |
| _End-to-end TPS_ | 1176.97 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 78.46 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 6.6ms |
| _P95_ | 6.0ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 213.68 tx/s |
| _Sustained TPS_ | 214.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 213.68 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.0 |
| _P99_ | 26.5ms |
| _P95_ | 26.4ms |
| _P50_ | 26.2ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 1122.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 74.82 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.6 |
| _P99_ | 15.4ms |
| _P95_ | 12.9ms |
| _P50_ | 6.8ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 130.58 tx/s |
| _Sustained TPS_ | 134.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 130.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.9 |
| _P99_ | 25.6ms |
| _P95_ | 25.6ms |
| _P50_ | 25.2ms |
| _Tx validation time p50 (ms)_ | 11.1 |
| _End-to-end TPS_ | 1161.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.44 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 6.2ms |
| _P95_ | 5.9ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 194.06 tx/s |
| _Sustained TPS_ | 190.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 194.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 52.4 |
| _P99_ | 54.0ms |
| _P95_ | 53.9ms |
| _P50_ | 52.8ms |
| _Tx validation time p50 (ms)_ | 23.3 |
| _End-to-end TPS_ | 1107.83 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 36.93 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.9 |
| _P99_ | 18.3ms |
| _P95_ | 16.0ms |
| _P50_ | 12.7ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 151.40 tx/s |
| _Sustained TPS_ | 151.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 151.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 62.3 |
| _P99_ | 63.1ms |
| _P95_ | 62.9ms |
| _P50_ | 62.5ms |
| _Tx validation time p50 (ms)_ | 21.1 |
| _End-to-end TPS_ | 936.49 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.22 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.3 |
| _P99_ | 111.3ms |
| _P95_ | 25.7ms |
| _P50_ | 19.3ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 88.45 tx/s |
| _Sustained TPS_ | 101.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 88.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 58.0 |
| _P99_ | 59.6ms |
| _P95_ | 59.6ms |
| _P50_ | 58.1ms |
| _Tx validation time p50 (ms)_ | 22.2 |
| _End-to-end TPS_ | 1002.72 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.42 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.7 |
| _P99_ | 23.4ms |
| _P95_ | 22.1ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 111.90 tx/s |
| _Sustained TPS_ | 106.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 111.90 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 119.8 |
| _P99_ | 122.3ms |
| _P95_ | 122.2ms |
| _P50_ | 119.2ms |
| _Tx validation time p50 (ms)_ | 47.9 |
| _End-to-end TPS_ | 734.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.33 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 40.5ms |
| _P95_ | 32.5ms |
| _P50_ | 24.1ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 118.72 tx/s |
| _Sustained TPS_ | 116.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 81.78 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 122.8 |
| _P99_ | 125.5ms |
| _P95_ | 125.3ms |
| _P50_ | 123.9ms |
| _Tx validation time p50 (ms)_ | 55.3 |
| _End-to-end TPS_ | 711.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.82 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.4 |
| _P99_ | 48.6ms |
| _P95_ | 41.9ms |
| _P50_ | 31.6ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 89.92 tx/s |
| _Sustained TPS_ | 91.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 61.94 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 124.5 |
| _P99_ | 126.4ms |
| _P95_ | 126.1ms |
| _P50_ | 124.3ms |
| _Tx validation time p50 (ms)_ | 42.8 |
| _End-to-end TPS_ | 706.45 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.70 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.2 |
| _P99_ | 48.7ms |
| _P95_ | 40.0ms |
| _P50_ | 31.2ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 94.92 tx/s |
| _Sustained TPS_ | 92.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.34 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
