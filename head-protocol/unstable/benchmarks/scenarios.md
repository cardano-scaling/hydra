--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-21 14:52:20.240681134 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 923.04 | n/a | 31.7 | 32.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 190.15 | 194.91 | 5.2 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 799.60 | n/a | 36.6 | 37.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 152.98 | 152.74 | 6.5 | 8.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 893.22 | n/a | 32.8 | 33.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 175.88 | 172.87 | 5.6 | 6.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 809.31 | n/a | 72.0 | 73.3 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 133.01 | 131.11 | 14.9 | 19.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 684.66 | n/a | 85.3 | 86.7 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 100.05 | 99.77 | 19.7 | 25.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 699.38 | n/a | 84.0 | 85.6 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 99.30 | 95.52 | 20.0 | 27.3 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 680.85 | n/a | 128.6 | 131.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 106.58 | 106.15 | 27.7 | 35.3 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 574.71 | n/a | 152.0 | 156.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 85.20 | 84.34 | 34.9 | 44.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 702.88 | n/a | 122.7 | 126.9 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 91.65 | 88.96 | 32.5 | 38.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.7 |
| _P99_ | 32.4ms |
| _P95_ | 32.4ms |
| _P50_ | 32.0ms |
| _Tx validation time p50 (ms)_ | 11.9 |
| _End-to-end TPS_ | 923.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.54 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.2 |
| _P99_ | 7.5ms |
| _P95_ | 5.9ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 190.15 tx/s |
| _Sustained TPS_ | 194.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 190.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 134.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.6 |
| _P99_ | 37.4ms |
| _P95_ | 37.3ms |
| _P50_ | 36.8ms |
| _Tx validation time p50 (ms)_ | 11.3 |
| _End-to-end TPS_ | 799.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 53.31 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.5 |
| _P99_ | 9.2ms |
| _P95_ | 8.9ms |
| _P50_ | 6.2ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 152.98 tx/s |
| _Sustained TPS_ | 152.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 152.98 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 132.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 33.4ms |
| _P95_ | 33.4ms |
| _P50_ | 33.0ms |
| _Tx validation time p50 (ms)_ | 11.9 |
| _End-to-end TPS_ | 893.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.55 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 134.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 7.2ms |
| _P95_ | 6.7ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 175.88 tx/s |
| _Sustained TPS_ | 172.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 175.88 /s |
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
| _Avg. Confirmation Time (ms)_ | 72.0 |
| _P99_ | 73.6ms |
| _P95_ | 73.3ms |
| _P50_ | 72.5ms |
| _Tx validation time p50 (ms)_ | 26.1 |
| _End-to-end TPS_ | 809.31 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.98 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.9 |
| _P99_ | 25.4ms |
| _P95_ | 19.8ms |
| _P50_ | 14.1ms |
| _Tx validation time p50 (ms)_ | 3.9 |
| _End-to-end TPS_ | 133.01 tx/s |
| _Sustained TPS_ | 131.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 133.01 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.3 |
| _P99_ | 87.1ms |
| _P95_ | 86.7ms |
| _P50_ | 85.7ms |
| _Tx validation time p50 (ms)_ | 30.6 |
| _End-to-end TPS_ | 684.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.82 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.7 |
| _P99_ | 27.0ms |
| _P95_ | 25.2ms |
| _P50_ | 19.9ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 100.05 tx/s |
| _Sustained TPS_ | 99.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.05 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 84.0 |
| _P99_ | 85.7ms |
| _P95_ | 85.6ms |
| _P50_ | 83.9ms |
| _Tx validation time p50 (ms)_ | 35.6 |
| _End-to-end TPS_ | 699.38 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.31 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
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
| _P99_ | 31.4ms |
| _P95_ | 27.3ms |
| _P50_ | 19.6ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 99.30 tx/s |
| _Sustained TPS_ | 95.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 99.30 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 128.6 |
| _P99_ | 131.3ms |
| _P95_ | 131.2ms |
| _P50_ | 128.1ms |
| _Tx validation time p50 (ms)_ | 54.1 |
| _End-to-end TPS_ | 680.85 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.13 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.7 |
| _P99_ | 36.3ms |
| _P95_ | 35.3ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 106.58 tx/s |
| _Sustained TPS_ | 106.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.24 /s |
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
| _Avg. Confirmation Time (ms)_ | 152.0 |
| _P99_ | 156.4ms |
| _P95_ | 156.3ms |
| _P50_ | 152.7ms |
| _Tx validation time p50 (ms)_ | 48.3 |
| _End-to-end TPS_ | 574.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.77 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.9 |
| _P99_ | 47.2ms |
| _P95_ | 44.0ms |
| _P50_ | 33.9ms |
| _Tx validation time p50 (ms)_ | 10.2 |
| _End-to-end TPS_ | 85.20 tx/s |
| _Sustained TPS_ | 84.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 56.80 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 122.7 |
| _P99_ | 127.3ms |
| _P95_ | 126.9ms |
| _P50_ | 123.3ms |
| _Tx validation time p50 (ms)_ | 67.2 |
| _End-to-end TPS_ | 702.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.62 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 48.1ms |
| _P95_ | 38.6ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 9.1 |
| _End-to-end TPS_ | 91.65 tx/s |
| _Sustained TPS_ | 88.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 61.10 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
