--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-29 14:47:00.348258107 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 856.84 | n/a | 34.2 | 34.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 185.29 | 191.38 | 5.3 | 6.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 732.28 | n/a | 40.1 | 40.7 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 144.03 | 145.53 | 6.8 | 9.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 791.14 | n/a | 36.9 | 37.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 165.88 | 163.92 | 5.9 | 7.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 714.54 | n/a | 81.6 | 83.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 115.10 | 112.43 | 17.2 | 22.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 684.82 | n/a | 85.4 | 87.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 91.88 | 90.51 | 21.5 | 26.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 723.31 | n/a | 81.2 | 82.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 95.63 | 93.32 | 20.7 | 25.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 616.12 | n/a | 143.4 | 144.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 100.99 | 100.28 | 29.3 | 37.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 615.91 | n/a | 140.5 | 144.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 72.50 | 74.46 | 41.0 | 50.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 637.69 | n/a | 136.9 | 140.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 86.01 | 83.14 | 34.4 | 41.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.2 |
| _P99_ | 34.8ms |
| _P95_ | 34.8ms |
| _P50_ | 34.4ms |
| _Tx validation time p50 (ms)_ | 14.0 |
| _End-to-end TPS_ | 856.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.12 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 8.1ms |
| _P95_ | 6.8ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 185.29 tx/s |
| _Sustained TPS_ | 191.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 185.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 141.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 40.1 |
| _P99_ | 40.8ms |
| _P95_ | 40.7ms |
| _P50_ | 40.4ms |
| _Tx validation time p50 (ms)_ | 13.0 |
| _End-to-end TPS_ | 732.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.82 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.8 |
| _P99_ | 10.3ms |
| _P95_ | 9.2ms |
| _P50_ | 6.6ms |
| _Tx validation time p50 (ms)_ | 2.0 |
| _End-to-end TPS_ | 144.03 tx/s |
| _Sustained TPS_ | 145.53 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 144.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.9 |
| _P99_ | 37.8ms |
| _P95_ | 37.8ms |
| _P50_ | 37.2ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 791.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 52.74 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 9.4ms |
| _P95_ | 7.9ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 165.88 tx/s |
| _Sustained TPS_ | 163.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 165.88 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.6 |
| _P99_ | 83.7ms |
| _P95_ | 83.7ms |
| _P50_ | 82.1ms |
| _Tx validation time p50 (ms)_ | 30.2 |
| _End-to-end TPS_ | 714.54 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.82 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 133.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.2 |
| _P99_ | 23.7ms |
| _P95_ | 22.3ms |
| _P50_ | 16.9ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 115.10 tx/s |
| _Sustained TPS_ | 112.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 115.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.4 |
| _P99_ | 87.4ms |
| _P95_ | 87.3ms |
| _P50_ | 85.3ms |
| _Tx validation time p50 (ms)_ | 34.3 |
| _End-to-end TPS_ | 684.82 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.83 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.5 |
| _P99_ | 29.7ms |
| _P95_ | 26.8ms |
| _P50_ | 21.5ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 91.88 tx/s |
| _Sustained TPS_ | 90.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 91.88 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.2 |
| _P99_ | 82.2ms |
| _P95_ | 82.2ms |
| _P50_ | 81.7ms |
| _Tx validation time p50 (ms)_ | 35.7 |
| _End-to-end TPS_ | 723.31 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.11 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.7 |
| _P99_ | 28.5ms |
| _P95_ | 25.9ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 5.7 |
| _End-to-end TPS_ | 95.63 tx/s |
| _Sustained TPS_ | 93.32 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.63 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.4 |
| _P99_ | 144.8ms |
| _P95_ | 144.8ms |
| _P50_ | 144.1ms |
| _Tx validation time p50 (ms)_ | 47.3 |
| _End-to-end TPS_ | 616.12 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.69 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.3 |
| _P99_ | 41.3ms |
| _P95_ | 37.2ms |
| _P50_ | 28.1ms |
| _Tx validation time p50 (ms)_ | 7.8 |
| _End-to-end TPS_ | 100.99 tx/s |
| _Sustained TPS_ | 100.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 68.45 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.5 |
| _P99_ | 144.9ms |
| _P95_ | 144.8ms |
| _P50_ | 144.1ms |
| _Tx validation time p50 (ms)_ | 67.3 |
| _End-to-end TPS_ | 615.91 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.69 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 41.0 |
| _P99_ | 51.3ms |
| _P95_ | 50.9ms |
| _P50_ | 39.2ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 72.50 tx/s |
| _Sustained TPS_ | 74.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 48.33 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 136.9 |
| _P99_ | 140.9ms |
| _P95_ | 140.8ms |
| _P50_ | 137.0ms |
| _Tx validation time p50 (ms)_ | 60.6 |
| _End-to-end TPS_ | 637.69 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.17 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.4 |
| _P99_ | 44.3ms |
| _P95_ | 41.5ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 86.01 tx/s |
| _Sustained TPS_ | 83.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.30 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
