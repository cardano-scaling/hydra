--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-23 08:30:46.131515186 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 942.90 | n/a | 30.8 | 31.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 202.36 | 202.07 | 4.9 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 915.48 | n/a | 31.8 | 32.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 134.82 | 133.51 | 7.3 | 10.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 964.64 | n/a | 30.2 | 30.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 175.14 | 171.58 | 5.7 | 7.1 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 814.78 | n/a | 72.1 | 73.4 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 127.98 | 129.44 | 15.4 | 20.6 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 756.56 | n/a | 77.6 | 78.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 96.41 | 95.58 | 20.5 | 24.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 750.78 | n/a | 78.1 | 79.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 100.99 | 96.95 | 19.6 | 26.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 606.45 | n/a | 145.8 | 147.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 106.39 | 106.53 | 27.8 | 34.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 561.55 | n/a | 156.9 | 160.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.61 | 83.35 | 35.5 | 44.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 598.73 | n/a | 147.5 | 149.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 85.56 | 85.52 | 34.5 | 44.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.8 |
| _P99_ | 31.6ms |
| _P95_ | 31.5ms |
| _P50_ | 31.1ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 942.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.86 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 5.8ms |
| _P95_ | 5.7ms |
| _P50_ | 4.7ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 202.36 tx/s |
| _Sustained TPS_ | 202.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 202.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.8 |
| _P99_ | 32.5ms |
| _P95_ | 32.5ms |
| _P50_ | 32.1ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 915.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.03 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.3 |
| _P99_ | 10.4ms |
| _P95_ | 10.4ms |
| _P50_ | 6.5ms |
| _Tx validation time p50 (ms)_ | 2.0 |
| _End-to-end TPS_ | 134.82 tx/s |
| _Sustained TPS_ | 133.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 134.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.2 |
| _P99_ | 30.9ms |
| _P95_ | 30.9ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 964.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.31 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.2ms |
| _P95_ | 7.1ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 175.14 tx/s |
| _Sustained TPS_ | 171.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 175.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.1 |
| _P99_ | 73.4ms |
| _P95_ | 73.4ms |
| _P50_ | 72.4ms |
| _Tx validation time p50 (ms)_ | 25.4 |
| _End-to-end TPS_ | 814.78 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.4 |
| _P99_ | 21.9ms |
| _P95_ | 20.6ms |
| _P50_ | 15.0ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 127.98 tx/s |
| _Sustained TPS_ | 129.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 127.98 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.6 |
| _P99_ | 78.5ms |
| _P95_ | 78.5ms |
| _P50_ | 77.9ms |
| _Tx validation time p50 (ms)_ | 32.0 |
| _End-to-end TPS_ | 756.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.22 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.5 |
| _P99_ | 27.1ms |
| _P95_ | 24.7ms |
| _P50_ | 20.6ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 96.41 tx/s |
| _Sustained TPS_ | 95.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.1 |
| _P99_ | 79.8ms |
| _P95_ | 79.7ms |
| _P50_ | 77.9ms |
| _Tx validation time p50 (ms)_ | 26.1 |
| _End-to-end TPS_ | 750.78 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.03 /s |
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
| _Avg. Confirmation Time (ms)_ | 19.6 |
| _P99_ | 27.1ms |
| _P95_ | 26.1ms |
| _P50_ | 19.1ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 100.99 tx/s |
| _Sustained TPS_ | 96.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.8 |
| _P99_ | 147.9ms |
| _P95_ | 147.8ms |
| _P50_ | 145.9ms |
| _Tx validation time p50 (ms)_ | 46.1 |
| _End-to-end TPS_ | 606.45 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.48 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.8 |
| _P99_ | 35.8ms |
| _P95_ | 34.2ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 106.39 tx/s |
| _Sustained TPS_ | 106.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.11 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 156.9 |
| _P99_ | 160.1ms |
| _P95_ | 160.0ms |
| _P50_ | 157.2ms |
| _Tx validation time p50 (ms)_ | 45.1 |
| _End-to-end TPS_ | 561.55 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.48 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.5 |
| _P99_ | 47.8ms |
| _P95_ | 44.0ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 83.61 tx/s |
| _Sustained TPS_ | 83.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 55.74 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.5 |
| _P99_ | 149.3ms |
| _P95_ | 149.2ms |
| _P50_ | 148.2ms |
| _Tx validation time p50 (ms)_ | 52.7 |
| _End-to-end TPS_ | 598.73 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.31 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.5 |
| _P99_ | 47.8ms |
| _P95_ | 44.4ms |
| _P50_ | 33.4ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 85.56 tx/s |
| _Sustained TPS_ | 85.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.99 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
