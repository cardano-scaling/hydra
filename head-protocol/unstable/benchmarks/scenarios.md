--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-27 12:24:12.685965623 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1216.30 | n/a | 24.1 | 24.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 204.74 | 215.82 | 4.8 | 7.4 |
| Nodes=1, Growing, fire and forget | 30 | 0.1 | 305.06 | n/a | 97.6 | 98.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.3 | 88.02 | 114.83 | 11.3 | 28.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1039.95 | n/a | 28.2 | 28.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 191.48 | 187.87 | 5.2 | 5.8 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 916.68 | n/a | 63.9 | 65.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 155.70 | 154.86 | 12.7 | 17.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 896.29 | n/a | 65.1 | 66.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 119.93 | 116.11 | 16.5 | 21.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1003.64 | n/a | 58.3 | 59.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 119.35 | 115.15 | 16.6 | 20.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 777.97 | n/a | 111.9 | 115.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 121.44 | 123.57 | 24.3 | 31.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 724.81 | n/a | 121.6 | 122.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 89.21 | 88.73 | 33.0 | 45.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 684.78 | n/a | 128.2 | 129.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 83.93 | 78.57 | 35.4 | 75.3 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 24.1 |
| _P99_ | 24.5ms |
| _P95_ | 24.5ms |
| _P50_ | 24.3ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 1216.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 81.09 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 11.4ms |
| _P95_ | 7.4ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 204.74 tx/s |
| _Sustained TPS_ | 215.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 204.74 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 97.6 |
| _P99_ | 98.2ms |
| _P95_ | 98.1ms |
| _P50_ | 97.9ms |
| _Tx validation time p50 (ms)_ | 82.2 |
| _End-to-end TPS_ | 305.06 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.34 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.3 |
| _P99_ | 66.0ms |
| _P95_ | 28.5ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 88.02 tx/s |
| _Sustained TPS_ | 114.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 88.02 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 28.2 |
| _P99_ | 28.7ms |
| _P95_ | 28.7ms |
| _P50_ | 28.4ms |
| _Tx validation time p50 (ms)_ | 11.5 |
| _End-to-end TPS_ | 1039.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 69.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 8.1ms |
| _P95_ | 5.8ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 191.48 tx/s |
| _Sustained TPS_ | 187.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 191.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.9 |
| _P99_ | 65.3ms |
| _P95_ | 65.2ms |
| _P50_ | 64.2ms |
| _Tx validation time p50 (ms)_ | 21.3 |
| _End-to-end TPS_ | 916.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.56 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.7 |
| _P99_ | 19.6ms |
| _P95_ | 17.4ms |
| _P50_ | 12.1ms |
| _Tx validation time p50 (ms)_ | 3.3 |
| _End-to-end TPS_ | 155.70 tx/s |
| _Sustained TPS_ | 154.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 155.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.1 |
| _P99_ | 66.8ms |
| _P95_ | 66.7ms |
| _P50_ | 65.4ms |
| _Tx validation time p50 (ms)_ | 25.3 |
| _End-to-end TPS_ | 896.29 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.88 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.5 |
| _P99_ | 26.1ms |
| _P95_ | 21.3ms |
| _P50_ | 16.1ms |
| _Tx validation time p50 (ms)_ | 4.7 |
| _End-to-end TPS_ | 119.93 tx/s |
| _Sustained TPS_ | 116.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 119.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 58.3 |
| _P99_ | 59.7ms |
| _P95_ | 59.6ms |
| _P50_ | 58.6ms |
| _Tx validation time p50 (ms)_ | 27.0 |
| _End-to-end TPS_ | 1003.64 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.45 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.6 |
| _P99_ | 22.3ms |
| _P95_ | 20.4ms |
| _P50_ | 17.0ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 119.35 tx/s |
| _Sustained TPS_ | 115.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 119.35 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 111.9 |
| _P99_ | 115.5ms |
| _P95_ | 115.4ms |
| _P50_ | 112.6ms |
| _Tx validation time p50 (ms)_ | 38.2 |
| _End-to-end TPS_ | 777.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.29 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 24.3 |
| _P99_ | 32.6ms |
| _P95_ | 31.9ms |
| _P50_ | 25.1ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 121.44 tx/s |
| _Sustained TPS_ | 123.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 82.31 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 121.6 |
| _P99_ | 122.5ms |
| _P95_ | 122.4ms |
| _P50_ | 122.0ms |
| _Tx validation time p50 (ms)_ | 44.7 |
| _End-to-end TPS_ | 724.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.11 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 173.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 96.6ms |
| _P95_ | 45.4ms |
| _P50_ | 30.4ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 89.21 tx/s |
| _Sustained TPS_ | 88.73 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.47 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.2 |
| _P99_ | 129.5ms |
| _P95_ | 129.5ms |
| _P50_ | 128.9ms |
| _Tx validation time p50 (ms)_ | 44.4 |
| _End-to-end TPS_ | 684.78 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.22 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.4 |
| _P99_ | 202.7ms |
| _P95_ | 75.3ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 83.93 tx/s |
| _Sustained TPS_ | 78.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.89 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
