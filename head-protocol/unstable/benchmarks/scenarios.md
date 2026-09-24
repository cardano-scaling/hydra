--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-24 12:47:52.310410925 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1042.83 | n/a | 27.8 | 28.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 196.12 | 196.18 | 5.0 | 6.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 899.24 | n/a | 32.6 | 33.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 164.92 | 165.59 | 6.0 | 7.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 898.24 | n/a | 32.5 | 33.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 177.41 | 176.54 | 5.6 | 7.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 742.40 | n/a | 79.1 | 80.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 131.33 | 129.75 | 15.1 | 19.7 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 819.87 | n/a | 70.8 | 72.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 99.32 | 99.55 | 19.5 | 24.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 835.06 | n/a | 70.1 | 71.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 106.11 | 102.16 | 18.7 | 23.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 615.80 | n/a | 143.0 | 144.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.18 | 103.85 | 28.4 | 36.3 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 564.15 | n/a | 157.1 | 158.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.77 | 83.46 | 35.1 | 44.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 602.55 | n/a | 145.1 | 148.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.46 | 84.15 | 33.7 | 42.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.8 |
| _P99_ | 28.5ms |
| _P95_ | 28.5ms |
| _P50_ | 28.0ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 1042.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 8.6ms |
| _P95_ | 6.5ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 196.12 tx/s |
| _Sustained TPS_ | 196.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 196.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.6 |
| _P99_ | 33.2ms |
| _P95_ | 33.1ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 10.7 |
| _End-to-end TPS_ | 899.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.95 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 7.9ms |
| _P95_ | 7.3ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 164.92 tx/s |
| _Sustained TPS_ | 165.59 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.92 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 33.2ms |
| _P95_ | 33.1ms |
| _P50_ | 32.7ms |
| _Tx validation time p50 (ms)_ | 11.7 |
| _End-to-end TPS_ | 898.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.88 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 7.8ms |
| _P95_ | 7.2ms |
| _P50_ | 5.3ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 177.41 tx/s |
| _Sustained TPS_ | 176.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.1 |
| _P99_ | 80.1ms |
| _P95_ | 80.0ms |
| _P50_ | 79.3ms |
| _Tx validation time p50 (ms)_ | 33.0 |
| _End-to-end TPS_ | 742.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.1 |
| _P99_ | 21.5ms |
| _P95_ | 19.7ms |
| _P50_ | 14.7ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 131.33 tx/s |
| _Sustained TPS_ | 129.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 131.33 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 133.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.8 |
| _P99_ | 72.9ms |
| _P95_ | 72.9ms |
| _P50_ | 71.9ms |
| _Tx validation time p50 (ms)_ | 17.7 |
| _End-to-end TPS_ | 819.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.33 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.5 |
| _P99_ | 27.0ms |
| _P95_ | 24.9ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 99.32 tx/s |
| _Sustained TPS_ | 99.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.32 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.1 |
| _P99_ | 71.7ms |
| _P95_ | 71.7ms |
| _P50_ | 69.8ms |
| _Tx validation time p50 (ms)_ | 25.5 |
| _End-to-end TPS_ | 835.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.84 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.7 |
| _P99_ | 24.4ms |
| _P95_ | 23.4ms |
| _P50_ | 18.3ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 106.11 tx/s |
| _Sustained TPS_ | 102.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 106.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.0 |
| _P99_ | 144.5ms |
| _P95_ | 144.4ms |
| _P50_ | 143.8ms |
| _Tx validation time p50 (ms)_ | 56.5 |
| _End-to-end TPS_ | 615.80 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.68 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.4 |
| _P99_ | 39.6ms |
| _P95_ | 36.3ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 104.18 tx/s |
| _Sustained TPS_ | 103.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 70.61 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 157.1 |
| _P99_ | 158.3ms |
| _P95_ | 158.1ms |
| _P50_ | 157.6ms |
| _Tx validation time p50 (ms)_ | 47.7 |
| _End-to-end TPS_ | 564.15 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.54 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 53.6ms |
| _P95_ | 44.2ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 83.77 tx/s |
| _Sustained TPS_ | 83.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.78 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.1 |
| _P99_ | 148.4ms |
| _P95_ | 148.2ms |
| _P50_ | 145.7ms |
| _Tx validation time p50 (ms)_ | 43.8 |
| _End-to-end TPS_ | 602.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.39 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.7 |
| _P99_ | 53.1ms |
| _P95_ | 42.3ms |
| _P50_ | 33.6ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 88.46 tx/s |
| _Sustained TPS_ | 84.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 59.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
