--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-28 12:54:46.253995732 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 915.81 | n/a | 31.9 | 32.6 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 192.56 | 191.74 | 5.1 | 6.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 801.55 | n/a | 36.5 | 37.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 152.49 | 149.00 | 6.5 | 8.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 933.93 | n/a | 31.3 | 31.9 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 169.16 | 164.58 | 5.8 | 7.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 794.85 | n/a | 73.6 | 75.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 122.98 | 123.37 | 16.1 | 20.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 725.00 | n/a | 80.7 | 82.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 95.01 | 94.65 | 20.8 | 26.1 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 714.35 | n/a | 82.0 | 83.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.21 | 96.10 | 20.0 | 27.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 677.82 | n/a | 129.9 | 131.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 106.11 | 105.54 | 27.9 | 34.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 553.38 | n/a | 159.5 | 162.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.34 | 83.31 | 34.9 | 42.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 651.68 | n/a | 134.0 | 137.8 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 84.00 | 82.42 | 35.1 | 45.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.9 |
| _P99_ | 32.6ms |
| _P95_ | 32.6ms |
| _P50_ | 32.1ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 915.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.05 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.2ms |
| _P95_ | 6.2ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 192.56 tx/s |
| _Sustained TPS_ | 191.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.5 |
| _P99_ | 37.2ms |
| _P95_ | 37.2ms |
| _P50_ | 36.8ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 801.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.44 /s |
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
| _Avg. Confirmation Time (ms)_ | 6.5 |
| _P99_ | 10.9ms |
| _P95_ | 8.8ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 152.49 tx/s |
| _Sustained TPS_ | 149.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 152.49 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 31.9ms |
| _P95_ | 31.9ms |
| _P50_ | 31.6ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 933.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.26 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 135.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 8.5ms |
| _P95_ | 7.6ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 169.16 tx/s |
| _Sustained TPS_ | 164.58 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 133.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.6 |
| _P99_ | 75.3ms |
| _P95_ | 75.2ms |
| _P50_ | 74.2ms |
| _Tx validation time p50 (ms)_ | 27.5 |
| _End-to-end TPS_ | 794.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.50 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.1 |
| _P99_ | 24.4ms |
| _P95_ | 20.9ms |
| _P50_ | 15.4ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 122.98 tx/s |
| _Sustained TPS_ | 123.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 122.98 /s |
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
| _Avg. Confirmation Time (ms)_ | 80.7 |
| _P99_ | 82.6ms |
| _P95_ | 82.5ms |
| _P50_ | 81.1ms |
| _Tx validation time p50 (ms)_ | 24.1 |
| _End-to-end TPS_ | 725.00 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.17 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.8 |
| _P99_ | 32.6ms |
| _P95_ | 26.1ms |
| _P50_ | 20.3ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 95.01 tx/s |
| _Sustained TPS_ | 94.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 95.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 82.0 |
| _P99_ | 83.8ms |
| _P95_ | 83.7ms |
| _P50_ | 82.5ms |
| _Tx validation time p50 (ms)_ | 33.0 |
| _End-to-end TPS_ | 714.35 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 30.2ms |
| _P95_ | 27.4ms |
| _P50_ | 19.6ms |
| _Tx validation time p50 (ms)_ | 6.7 |
| _End-to-end TPS_ | 99.21 tx/s |
| _Sustained TPS_ | 96.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.21 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 129.9 |
| _P99_ | 131.6ms |
| _P95_ | 131.5ms |
| _P50_ | 130.7ms |
| _Tx validation time p50 (ms)_ | 66.7 |
| _End-to-end TPS_ | 677.82 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.06 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.9 |
| _P99_ | 38.0ms |
| _P95_ | 34.2ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 106.11 tx/s |
| _Sustained TPS_ | 105.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 71.92 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 159.5 |
| _P99_ | 162.1ms |
| _P95_ | 162.0ms |
| _P50_ | 161.4ms |
| _Tx validation time p50 (ms)_ | 75.7 |
| _End-to-end TPS_ | 553.38 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.30 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 44.3ms |
| _P95_ | 42.5ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 84.34 tx/s |
| _Sustained TPS_ | 83.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.17 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 134.0 |
| _P99_ | 137.9ms |
| _P95_ | 137.8ms |
| _P50_ | 136.0ms |
| _Tx validation time p50 (ms)_ | 57.5 |
| _End-to-end TPS_ | 651.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.48 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 138.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.1 |
| _P99_ | 49.5ms |
| _P95_ | 45.6ms |
| _P50_ | 35.2ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 84.00 tx/s |
| _Sustained TPS_ | 82.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.94 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
