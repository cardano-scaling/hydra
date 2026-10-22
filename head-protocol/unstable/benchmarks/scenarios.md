--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-22 10:59:48.134530631 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1147.50 | n/a | 25.3 | 25.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 189.17 | 187.67 | 5.2 | 7.2 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 841.19 | n/a | 34.7 | 35.5 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 164.12 | 162.87 | 6.0 | 7.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 956.14 | n/a | 30.5 | 31.2 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 163.59 | 159.75 | 6.1 | 8.0 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 858.55 | n/a | 68.0 | 69.1 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 120.61 | 121.83 | 16.4 | 21.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 695.56 | n/a | 83.4 | 86.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 97.36 | 95.86 | 20.3 | 26.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 757.36 | n/a | 77.3 | 79.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 101.69 | 98.75 | 19.5 | 25.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 602.87 | n/a | 144.3 | 149.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 101.82 | 99.74 | 29.0 | 37.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 583.88 | n/a | 150.2 | 153.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.59 | 82.63 | 35.6 | 42.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 674.70 | n/a | 130.2 | 132.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.99 | 86.95 | 33.2 | 42.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.3 |
| _P99_ | 26.0ms |
| _P95_ | 25.9ms |
| _P50_ | 25.5ms |
| _Tx validation time p50 (ms)_ | 15.3 |
| _End-to-end TPS_ | 1147.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.50 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
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
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 189.17 tx/s |
| _Sustained TPS_ | 187.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 189.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.7 |
| _P99_ | 35.6ms |
| _P95_ | 35.5ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 12.7 |
| _End-to-end TPS_ | 841.19 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 56.08 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 133.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.0 |
| _P99_ | 7.8ms |
| _P95_ | 7.1ms |
| _P50_ | 5.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 164.12 tx/s |
| _Sustained TPS_ | 162.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 164.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.5 |
| _P99_ | 31.2ms |
| _P95_ | 31.2ms |
| _P50_ | 30.7ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 956.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.74 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.1 |
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
| _P99_ | 9.0ms |
| _P95_ | 8.0ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 163.59 tx/s |
| _Sustained TPS_ | 159.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 163.59 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 68.0 |
| _P99_ | 69.2ms |
| _P95_ | 69.1ms |
| _P50_ | 68.4ms |
| _Tx validation time p50 (ms)_ | 21.0 |
| _End-to-end TPS_ | 858.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.62 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.4 |
| _P99_ | 24.1ms |
| _P95_ | 21.3ms |
| _P50_ | 16.1ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 120.61 tx/s |
| _Sustained TPS_ | 121.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 120.61 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 83.4 |
| _P99_ | 86.1ms |
| _P95_ | 86.0ms |
| _P50_ | 83.7ms |
| _Tx validation time p50 (ms)_ | 25.8 |
| _End-to-end TPS_ | 695.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.19 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.3 |
| _P99_ | 27.9ms |
| _P95_ | 26.4ms |
| _P50_ | 19.8ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 97.36 tx/s |
| _Sustained TPS_ | 95.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 97.36 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 77.3 |
| _P99_ | 79.2ms |
| _P95_ | 79.1ms |
| _P50_ | 77.7ms |
| _Tx validation time p50 (ms)_ | 27.1 |
| _End-to-end TPS_ | 757.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.25 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 130.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.5 |
| _P99_ | 28.9ms |
| _P95_ | 25.3ms |
| _P50_ | 19.2ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 101.69 tx/s |
| _Sustained TPS_ | 98.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 101.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.3 |
| _P99_ | 149.1ms |
| _P95_ | 149.0ms |
| _P50_ | 146.5ms |
| _Tx validation time p50 (ms)_ | 45.5 |
| _End-to-end TPS_ | 602.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.40 /s |
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
| _Avg. Confirmation Time (ms)_ | 29.0 |
| _P99_ | 37.8ms |
| _P95_ | 37.2ms |
| _P50_ | 28.8ms |
| _Tx validation time p50 (ms)_ | 7.6 |
| _End-to-end TPS_ | 101.82 tx/s |
| _Sustained TPS_ | 99.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 69.01 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 150.2 |
| _P99_ | 153.9ms |
| _P95_ | 153.8ms |
| _P50_ | 150.4ms |
| _Tx validation time p50 (ms)_ | 61.0 |
| _End-to-end TPS_ | 583.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.98 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 54.7ms |
| _P95_ | 42.5ms |
| _P50_ | 36.0ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 83.59 tx/s |
| _Sustained TPS_ | 82.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.65 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 130.2 |
| _P99_ | 132.3ms |
| _P95_ | 132.2ms |
| _P50_ | 131.7ms |
| _Tx validation time p50 (ms)_ | 58.1 |
| _End-to-end TPS_ | 674.70 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.99 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 46.7ms |
| _P95_ | 42.3ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 88.99 tx/s |
| _Sustained TPS_ | 86.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.32 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
