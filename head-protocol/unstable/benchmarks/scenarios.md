--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-27 09:03:31.26031319 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 730.64 | n/a | 40.1 | 40.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 190.52 | 190.48 | 5.2 | 7.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 722.54 | n/a | 40.4 | 41.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 160.85 | 158.38 | 6.1 | 7.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 929.77 | n/a | 31.4 | 32.0 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 157.41 | 164.56 | 6.3 | 8.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 750.70 | n/a | 78.0 | 79.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 119.28 | 117.93 | 16.5 | 22.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 723.24 | n/a | 81.0 | 82.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 95.87 | 95.71 | 20.6 | 25.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 716.29 | n/a | 81.5 | 83.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 92.80 | 89.69 | 21.4 | 27.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 605.17 | n/a | 145.4 | 147.9 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.66 | 104.64 | 28.4 | 35.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 650.98 | n/a | 134.6 | 137.9 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 80.02 | 80.62 | 36.6 | 43.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 625.64 | n/a | 140.6 | 143.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 86.62 | 86.00 | 34.2 | 41.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 40.1 |
| _P99_ | 40.8ms |
| _P95_ | 40.8ms |
| _P50_ | 40.4ms |
| _Tx validation time p50 (ms)_ | 13.0 |
| _End-to-end TPS_ | 730.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.71 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 8.5ms |
| _P95_ | 7.2ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 190.52 tx/s |
| _Sustained TPS_ | 190.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 190.52 /s |
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
| _Avg. Confirmation Time (ms)_ | 40.4 |
| _P99_ | 41.3ms |
| _P95_ | 41.3ms |
| _P50_ | 40.7ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 722.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.17 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 8.1ms |
| _P95_ | 7.7ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 160.85 tx/s |
| _Sustained TPS_ | 158.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 160.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.4 |
| _P99_ | 32.1ms |
| _P95_ | 32.0ms |
| _P50_ | 31.6ms |
| _Tx validation time p50 (ms)_ | 12.5 |
| _End-to-end TPS_ | 929.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.98 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.3 |
| _P99_ | 12.4ms |
| _P95_ | 8.3ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 157.41 tx/s |
| _Sustained TPS_ | 164.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 157.41 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.0 |
| _P99_ | 79.7ms |
| _P95_ | 79.6ms |
| _P50_ | 78.6ms |
| _Tx validation time p50 (ms)_ | 23.5 |
| _End-to-end TPS_ | 750.70 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.02 /s |
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
| _Avg. Confirmation Time (ms)_ | 16.5 |
| _P99_ | 24.6ms |
| _P95_ | 22.5ms |
| _P50_ | 16.2ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 119.28 tx/s |
| _Sustained TPS_ | 117.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 119.28 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.0 |
| _P99_ | 82.1ms |
| _P95_ | 82.1ms |
| _P50_ | 81.5ms |
| _Tx validation time p50 (ms)_ | 25.6 |
| _End-to-end TPS_ | 723.24 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.11 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.6 |
| _P99_ | 32.6ms |
| _P95_ | 25.8ms |
| _P50_ | 20.5ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 95.87 tx/s |
| _Sustained TPS_ | 95.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.5 |
| _P99_ | 83.6ms |
| _P95_ | 83.5ms |
| _P50_ | 82.1ms |
| _Tx validation time p50 (ms)_ | 25.8 |
| _End-to-end TPS_ | 716.29 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.88 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.4 |
| _P99_ | 29.5ms |
| _P95_ | 27.8ms |
| _P50_ | 20.9ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 92.80 tx/s |
| _Sustained TPS_ | 89.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.80 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 145.4 |
| _P99_ | 148.0ms |
| _P95_ | 147.9ms |
| _P50_ | 146.7ms |
| _Tx validation time p50 (ms)_ | 51.9 |
| _End-to-end TPS_ | 605.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.45 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.4 |
| _P99_ | 36.1ms |
| _P95_ | 35.1ms |
| _P50_ | 28.1ms |
| _Tx validation time p50 (ms)_ | 7.6 |
| _End-to-end TPS_ | 104.66 tx/s |
| _Sustained TPS_ | 104.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 69.77 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 134.6 |
| _P99_ | 138.0ms |
| _P95_ | 137.9ms |
| _P50_ | 135.8ms |
| _Tx validation time p50 (ms)_ | 63.0 |
| _End-to-end TPS_ | 650.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.47 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.6 |
| _P99_ | 49.1ms |
| _P95_ | 43.7ms |
| _P50_ | 36.2ms |
| _Tx validation time p50 (ms)_ | 10.7 |
| _End-to-end TPS_ | 80.02 tx/s |
| _Sustained TPS_ | 80.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.24 /s |
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
| _Avg. Confirmation Time (ms)_ | 140.6 |
| _P99_ | 143.1ms |
| _P95_ | 143.0ms |
| _P50_ | 140.8ms |
| _Tx validation time p50 (ms)_ | 74.6 |
| _End-to-end TPS_ | 625.64 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.90 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.2 |
| _P99_ | 43.7ms |
| _P95_ | 41.6ms |
| _P50_ | 34.1ms |
| _Tx validation time p50 (ms)_ | 8.6 |
| _End-to-end TPS_ | 86.62 tx/s |
| _Sustained TPS_ | 86.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.71 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
