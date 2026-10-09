--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-09 08:10:22.130921444 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1020.05 | n/a | 28.4 | 29.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 189.87 | 193.61 | 5.2 | 7.3 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 903.12 | n/a | 32.3 | 33.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 150.60 | 147.79 | 6.6 | 9.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 953.72 | n/a | 30.6 | 31.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 169.16 | 167.13 | 5.8 | 8.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 756.81 | n/a | 77.7 | 79.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 128.18 | 128.13 | 15.4 | 18.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 675.43 | n/a | 86.5 | 88.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 97.72 | 96.47 | 20.2 | 27.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 778.26 | n/a | 75.1 | 76.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 102.59 | 101.83 | 19.3 | 24.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 577.68 | n/a | 150.8 | 152.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 101.70 | 102.23 | 29.0 | 38.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 596.84 | n/a | 147.0 | 150.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.68 | 83.80 | 34.8 | 47.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 703.11 | n/a | 123.7 | 127.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 89.16 | 86.94 | 33.1 | 41.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.4 |
| _P99_ | 29.2ms |
| _P95_ | 29.2ms |
| _P50_ | 28.6ms |
| _Tx validation time p50 (ms)_ | 13.8 |
| _End-to-end TPS_ | 1020.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 68.00 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 7.7ms |
| _P95_ | 7.3ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 189.87 tx/s |
| _Sustained TPS_ | 193.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 189.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.3 |
| _P99_ | 33.0ms |
| _P95_ | 33.0ms |
| _P50_ | 32.6ms |
| _Tx validation time p50 (ms)_ | 14.3 |
| _End-to-end TPS_ | 903.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.21 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.6 |
| _P99_ | 10.2ms |
| _P95_ | 9.8ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 150.60 tx/s |
| _Sustained TPS_ | 147.79 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 150.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.6 |
| _P99_ | 31.3ms |
| _P95_ | 31.2ms |
| _P50_ | 30.8ms |
| _Tx validation time p50 (ms)_ | 10.8 |
| _End-to-end TPS_ | 953.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.58 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.7 |
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
| _P99_ | 8.4ms |
| _P95_ | 8.3ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 169.16 tx/s |
| _Sustained TPS_ | 167.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.7 |
| _P99_ | 79.2ms |
| _P95_ | 79.0ms |
| _P50_ | 78.3ms |
| _Tx validation time p50 (ms)_ | 27.3 |
| _End-to-end TPS_ | 756.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.23 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.4 |
| _P99_ | 21.4ms |
| _P95_ | 18.5ms |
| _P50_ | 15.3ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 128.18 tx/s |
| _Sustained TPS_ | 128.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 128.18 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.5 |
| _P99_ | 88.6ms |
| _P95_ | 88.6ms |
| _P50_ | 87.1ms |
| _Tx validation time p50 (ms)_ | 29.0 |
| _End-to-end TPS_ | 675.43 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.51 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.2 |
| _P99_ | 32.4ms |
| _P95_ | 27.2ms |
| _P50_ | 19.4ms |
| _Tx validation time p50 (ms)_ | 5.0 |
| _End-to-end TPS_ | 97.72 tx/s |
| _Sustained TPS_ | 96.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.72 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 135.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 75.1 |
| _P99_ | 76.6ms |
| _P95_ | 76.2ms |
| _P50_ | 75.4ms |
| _Tx validation time p50 (ms)_ | 34.7 |
| _End-to-end TPS_ | 778.26 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.94 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.3 |
| _P99_ | 27.1ms |
| _P95_ | 24.8ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 102.59 tx/s |
| _Sustained TPS_ | 101.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.59 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 150.8 |
| _P99_ | 152.9ms |
| _P95_ | 152.8ms |
| _P50_ | 151.8ms |
| _Tx validation time p50 (ms)_ | 57.7 |
| _End-to-end TPS_ | 577.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.84 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.0 |
| _P99_ | 42.8ms |
| _P95_ | 38.6ms |
| _P50_ | 28.2ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 101.70 tx/s |
| _Sustained TPS_ | 102.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 68.93 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.0 |
| _P99_ | 150.6ms |
| _P95_ | 150.5ms |
| _P50_ | 148.0ms |
| _Tx validation time p50 (ms)_ | 46.6 |
| _End-to-end TPS_ | 596.84 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.26 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 50.4ms |
| _P95_ | 47.0ms |
| _P50_ | 34.7ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 84.68 tx/s |
| _Sustained TPS_ | 83.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 58.33 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 123.7 |
| _P99_ | 127.4ms |
| _P95_ | 127.3ms |
| _P50_ | 122.6ms |
| _Tx validation time p50 (ms)_ | 61.2 |
| _End-to-end TPS_ | 703.11 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.62 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.1 |
| _P99_ | 46.3ms |
| _P95_ | 41.9ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 89.16 tx/s |
| _Sustained TPS_ | 86.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.43 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
