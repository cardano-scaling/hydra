--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-29 10:16:36.143269066 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 980.64 | n/a | 29.7 | 30.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 188.61 | 188.37 | 5.2 | 6.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 760.80 | n/a | 38.3 | 39.2 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 142.89 | 138.55 | 6.9 | 10.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 979.77 | n/a | 29.7 | 30.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 172.71 | 173.27 | 5.7 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 801.48 | n/a | 72.7 | 73.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 123.57 | 120.27 | 16.0 | 22.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 687.70 | n/a | 85.5 | 87.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 96.04 | 94.05 | 20.6 | 25.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 790.46 | n/a | 73.5 | 75.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.12 | 95.62 | 20.0 | 27.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 601.09 | n/a | 146.9 | 149.5 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.99 | 106.71 | 28.1 | 35.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 559.06 | n/a | 157.2 | 160.7 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 83.08 | 83.05 | 35.5 | 43.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 572.32 | n/a | 154.0 | 156.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 86.72 | 84.84 | 34.3 | 44.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.7 |
| _P99_ | 30.4ms |
| _P95_ | 30.4ms |
| _P50_ | 30.0ms |
| _Tx validation time p50 (ms)_ | 11.2 |
| _End-to-end TPS_ | 980.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.38 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 126.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 8.3ms |
| _P95_ | 6.7ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 188.61 tx/s |
| _Sustained TPS_ | 188.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 188.61 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 136.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 38.3 |
| _P99_ | 39.2ms |
| _P95_ | 39.2ms |
| _P50_ | 38.6ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 760.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 50.72 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 133.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.9 |
| _P99_ | 11.4ms |
| _P95_ | 10.5ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 142.89 tx/s |
| _Sustained TPS_ | 138.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 142.89 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.7 |
| _P99_ | 30.5ms |
| _P95_ | 30.4ms |
| _P50_ | 30.0ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 979.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.32 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.7 |
| _P99_ | 7.5ms |
| _P95_ | 6.5ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 172.71 tx/s |
| _Sustained TPS_ | 173.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 172.71 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 133.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.7 |
| _P99_ | 73.5ms |
| _P95_ | 73.5ms |
| _P50_ | 73.0ms |
| _Tx validation time p50 (ms)_ | 21.7 |
| _End-to-end TPS_ | 801.48 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.72 /s |
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
| _Avg. Confirmation Time (ms)_ | 16.0 |
| _P99_ | 25.7ms |
| _P95_ | 22.2ms |
| _P50_ | 15.7ms |
| _Tx validation time p50 (ms)_ | 4.0 |
| _End-to-end TPS_ | 123.57 tx/s |
| _Sustained TPS_ | 120.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 123.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.5 |
| _P99_ | 87.1ms |
| _P95_ | 87.0ms |
| _P50_ | 86.0ms |
| _Tx validation time p50 (ms)_ | 32.2 |
| _End-to-end TPS_ | 687.70 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.92 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 137.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.6 |
| _P99_ | 36.3ms |
| _P95_ | 25.8ms |
| _P50_ | 20.5ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 96.04 tx/s |
| _Sustained TPS_ | 94.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.04 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.5 |
| _P99_ | 75.7ms |
| _P95_ | 75.6ms |
| _P50_ | 74.1ms |
| _Tx validation time p50 (ms)_ | 23.9 |
| _End-to-end TPS_ | 790.46 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.35 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 128.5 |
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
| _P99_ | 30.1ms |
| _P95_ | 27.8ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 99.12 tx/s |
| _Sustained TPS_ | 95.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.9 |
| _P99_ | 149.6ms |
| _P95_ | 149.5ms |
| _P50_ | 147.5ms |
| _Tx validation time p50 (ms)_ | 53.2 |
| _End-to-end TPS_ | 601.09 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.36 /s |
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
| _Avg. Confirmation Time (ms)_ | 28.1 |
| _P99_ | 41.2ms |
| _P95_ | 35.6ms |
| _P50_ | 27.5ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 104.99 tx/s |
| _Sustained TPS_ | 106.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 71.16 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 157.2 |
| _P99_ | 160.8ms |
| _P95_ | 160.7ms |
| _P50_ | 157.5ms |
| _Tx validation time p50 (ms)_ | 56.4 |
| _End-to-end TPS_ | 559.06 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.42 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.5 |
| _P99_ | 47.2ms |
| _P95_ | 43.5ms |
| _P50_ | 35.4ms |
| _Tx validation time p50 (ms)_ | 9.9 |
| _End-to-end TPS_ | 83.08 tx/s |
| _Sustained TPS_ | 83.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.31 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.0 |
| _P99_ | 156.6ms |
| _P95_ | 156.5ms |
| _P50_ | 155.6ms |
| _Tx validation time p50 (ms)_ | 54.9 |
| _End-to-end TPS_ | 572.32 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.72 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.3 |
| _P99_ | 47.6ms |
| _P95_ | 44.8ms |
| _P50_ | 33.6ms |
| _Tx validation time p50 (ms)_ | 10.6 |
| _End-to-end TPS_ | 86.72 tx/s |
| _Sustained TPS_ | 84.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 58.78 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
