--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-15 08:50:15.135001319 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 815.09 | n/a | 36.0 | 36.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 193.10 | 192.30 | 5.1 | 6.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 827.60 | n/a | 35.2 | 36.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 151.50 | 148.84 | 6.5 | 8.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 915.60 | n/a | 31.8 | 32.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 174.57 | 171.85 | 5.7 | 7.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 824.62 | n/a | 69.7 | 72.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 125.14 | 128.82 | 15.7 | 20.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 774.18 | n/a | 75.9 | 76.8 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 92.73 | 90.44 | 21.1 | 26.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 754.94 | n/a | 77.0 | 79.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 100.19 | 96.23 | 19.7 | 26.0 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 572.74 | n/a | 154.4 | 156.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 109.12 | 108.17 | 27.2 | 33.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 519.51 | n/a | 169.7 | 172.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.14 | 81.50 | 35.5 | 46.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 607.40 | n/a | 144.3 | 146.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 94.70 | 91.26 | 31.4 | 41.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.0 |
| _P99_ | 36.5ms |
| _P95_ | 36.5ms |
| _P50_ | 36.2ms |
| _Tx validation time p50 (ms)_ | 12.1 |
| _End-to-end TPS_ | 815.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.34 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.0ms |
| _P95_ | 6.6ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 193.10 tx/s |
| _Sustained TPS_ | 192.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 193.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 36.1ms |
| _P95_ | 36.1ms |
| _P50_ | 35.5ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 827.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 55.17 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.5 |
| _P99_ | 9.3ms |
| _P95_ | 8.6ms |
| _P50_ | 6.4ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 151.50 tx/s |
| _Sustained TPS_ | 148.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 151.50 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.8 |
| _P99_ | 32.6ms |
| _P95_ | 32.5ms |
| _P50_ | 32.1ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 915.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.04 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 8.3ms |
| _P95_ | 7.4ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 174.57 tx/s |
| _Sustained TPS_ | 171.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 174.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 69.7 |
| _P99_ | 72.5ms |
| _P95_ | 72.2ms |
| _P50_ | 71.0ms |
| _Tx validation time p50 (ms)_ | 29.9 |
| _End-to-end TPS_ | 824.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.49 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.7 |
| _P99_ | 26.3ms |
| _P95_ | 20.9ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 125.14 tx/s |
| _Sustained TPS_ | 128.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 125.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 75.9 |
| _P99_ | 76.9ms |
| _P95_ | 76.8ms |
| _P50_ | 76.3ms |
| _Tx validation time p50 (ms)_ | 28.8 |
| _End-to-end TPS_ | 774.18 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.1 |
| _P99_ | 29.8ms |
| _P95_ | 26.8ms |
| _P50_ | 21.1ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 92.73 tx/s |
| _Sustained TPS_ | 90.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.73 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.0 |
| _P99_ | 79.3ms |
| _P95_ | 79.2ms |
| _P50_ | 77.4ms |
| _Tx validation time p50 (ms)_ | 28.0 |
| _End-to-end TPS_ | 754.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.7 |
| _P99_ | 27.1ms |
| _P95_ | 26.0ms |
| _P50_ | 19.3ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 100.19 tx/s |
| _Sustained TPS_ | 96.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.19 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.4 |
| _P99_ | 156.9ms |
| _P95_ | 156.8ms |
| _P50_ | 155.0ms |
| _Tx validation time p50 (ms)_ | 44.0 |
| _End-to-end TPS_ | 572.74 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.73 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 36.4ms |
| _P95_ | 33.5ms |
| _P50_ | 26.6ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 109.12 tx/s |
| _Sustained TPS_ | 108.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 72.75 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 169.7 |
| _P99_ | 172.2ms |
| _P95_ | 172.0ms |
| _P50_ | 169.7ms |
| _Tx validation time p50 (ms)_ | 47.9 |
| _End-to-end TPS_ | 519.51 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.54 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.5 |
| _P99_ | 50.9ms |
| _P95_ | 46.5ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 83.14 tx/s |
| _Sustained TPS_ | 81.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.35 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.3 |
| _P99_ | 146.6ms |
| _P95_ | 146.5ms |
| _P50_ | 145.0ms |
| _Tx validation time p50 (ms)_ | 52.0 |
| _End-to-end TPS_ | 607.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.50 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.4 |
| _P99_ | 46.3ms |
| _P95_ | 41.8ms |
| _P50_ | 31.1ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 94.70 tx/s |
| _Sustained TPS_ | 91.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 63.13 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
