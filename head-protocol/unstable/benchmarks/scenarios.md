--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-19 14:02:57.24383393 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 962.16 | n/a | 30.3 | 30.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 198.83 | 200.78 | 5.0 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 863.95 | n/a | 33.9 | 34.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 163.59 | 165.30 | 6.1 | 7.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 928.65 | n/a | 31.4 | 32.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 171.86 | 169.81 | 5.8 | 6.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 824.51 | n/a | 70.9 | 72.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 130.47 | 131.99 | 15.2 | 18.0 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 733.19 | n/a | 80.0 | 81.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 98.50 | 98.86 | 20.0 | 30.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 745.67 | n/a | 78.7 | 80.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 104.11 | 101.81 | 19.0 | 24.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 695.46 | n/a | 126.6 | 129.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.44 | 105.46 | 28.2 | 38.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 564.85 | n/a | 155.3 | 159.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.94 | 83.43 | 35.2 | 41.6 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 612.75 | n/a | 143.9 | 146.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.85 | 86.37 | 33.3 | 43.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.3 |
| _P99_ | 31.0ms |
| _P95_ | 30.9ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 962.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 64.14 /s |
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
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 6.7ms |
| _P95_ | 5.9ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 198.83 tx/s |
| _Sustained TPS_ | 200.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 198.83 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 34.6ms |
| _P95_ | 34.6ms |
| _P50_ | 34.2ms |
| _Tx validation time p50 (ms)_ | 12.2 |
| _End-to-end TPS_ | 863.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 57.60 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 7.4ms |
| _P95_ | 7.3ms |
| _P50_ | 6.0ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 163.59 tx/s |
| _Sustained TPS_ | 165.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 163.59 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.4 |
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
| _P95_ | 32.1ms |
| _P50_ | 31.6ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 928.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.91 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.8 |
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
| _P99_ | 7.7ms |
| _P95_ | 6.9ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 171.86 tx/s |
| _Sustained TPS_ | 169.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.86 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.9 |
| _P99_ | 72.7ms |
| _P95_ | 72.5ms |
| _P50_ | 71.2ms |
| _Tx validation time p50 (ms)_ | 23.9 |
| _End-to-end TPS_ | 824.51 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.48 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.2 |
| _P99_ | 20.4ms |
| _P95_ | 18.0ms |
| _P50_ | 14.6ms |
| _Tx validation time p50 (ms)_ | 5.0 |
| _End-to-end TPS_ | 130.47 tx/s |
| _Sustained TPS_ | 131.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 130.47 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.0 |
| _P99_ | 81.6ms |
| _P95_ | 81.6ms |
| _P50_ | 80.5ms |
| _Tx validation time p50 (ms)_ | 29.5 |
| _End-to-end TPS_ | 733.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.44 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.0 |
| _P99_ | 31.8ms |
| _P95_ | 30.0ms |
| _P50_ | 18.7ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 98.50 tx/s |
| _Sustained TPS_ | 98.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 98.50 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 78.7 |
| _P99_ | 80.3ms |
| _P95_ | 80.2ms |
| _P50_ | 79.1ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 745.67 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.86 /s |
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
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 27.2ms |
| _P95_ | 24.5ms |
| _P50_ | 19.2ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 104.11 tx/s |
| _Sustained TPS_ | 101.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 141.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 126.6 |
| _P99_ | 129.2ms |
| _P95_ | 129.1ms |
| _P50_ | 127.0ms |
| _Tx validation time p50 (ms)_ | 43.0 |
| _End-to-end TPS_ | 695.46 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.45 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.2 |
| _P99_ | 41.2ms |
| _P95_ | 38.1ms |
| _P50_ | 28.0ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 104.44 tx/s |
| _Sustained TPS_ | 105.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 71.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 155.3 |
| _P99_ | 159.2ms |
| _P95_ | 159.1ms |
| _P50_ | 155.9ms |
| _Tx validation time p50 (ms)_ | 75.7 |
| _End-to-end TPS_ | 564.85 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.55 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.2 |
| _P99_ | 44.4ms |
| _P95_ | 41.6ms |
| _P50_ | 35.1ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 83.94 tx/s |
| _Sustained TPS_ | 83.43 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.89 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.9 |
| _P99_ | 146.5ms |
| _P95_ | 146.3ms |
| _P50_ | 143.8ms |
| _Tx validation time p50 (ms)_ | 55.2 |
| _End-to-end TPS_ | 612.75 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.62 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.3 |
| _P99_ | 46.6ms |
| _P95_ | 43.1ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 88.85 tx/s |
| _Sustained TPS_ | 86.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.22 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
