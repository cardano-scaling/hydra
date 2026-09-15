--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-15 22:02:56.696334644 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.1 | 208.68 | n/a | 143.1 | 143.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.4 | 77.34 | 72.33 | 12.9 | 48.6 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1144.24 | n/a | 25.6 | 26.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.7 | 45.52 | 50.87 | 21.9 | 157.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1312.15 | n/a | 22.3 | 22.6 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.8 | 39.40 | 32.75 | 25.3 | 123.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.7 | 85.38 | n/a | 699.6 | 702.2 |
| Nodes=2, Constant, wait for tx valid | 60 | 1.6 | 37.45 | 33.29 | 53.3 | 206.9 |
| Nodes=2, Growing, fire and forget | 60 | 0.3 | 220.82 | n/a | 270.5 | 271.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 4.6 | 13.06 | 11.82 | 153.0 | 494.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 576.25 | n/a | 103.0 | 104.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 1.9 | 31.82 | 32.56 | 62.5 | 190.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 413.26 | n/a | 215.7 | 217.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 3.6 | 24.80 | 27.27 | 120.7 | 430.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.3 | 260.59 | n/a | 343.4 | 345.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 3.6 | 24.78 | 31.19 | 115.5 | 280.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 637.18 | n/a | 139.4 | 140.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.4 | 63.45 | 69.17 | 46.7 | 172.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 143.1 |
| _P99_ | 143.5ms |
| _P95_ | 143.5ms |
| _P50_ | 143.3ms |
| _Tx validation time p50 (ms)_ | 38.6 |
| _End-to-end TPS_ | 208.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.91 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.9 |
| _P99_ | 57.3ms |
| _P95_ | 48.6ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 77.34 tx/s |
| _Sustained TPS_ | 72.33 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 77.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.6 |
| _P99_ | 26.0ms |
| _P95_ | 26.0ms |
| _P50_ | 25.9ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 1144.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.28 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 130.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.9 |
| _P99_ | 178.6ms |
| _P95_ | 157.7ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 45.52 tx/s |
| _Sustained TPS_ | 50.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 45.52 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 22.3 |
| _P99_ | 22.7ms |
| _P95_ | 22.6ms |
| _P50_ | 22.5ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 1312.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 87.48 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.3 |
| _P99_ | 198.8ms |
| _P95_ | 123.5ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 39.40 tx/s |
| _Sustained TPS_ | 32.75 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 39.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 699.6 |
| _P99_ | 702.2ms |
| _P95_ | 702.2ms |
| _P50_ | 701.9ms |
| _Tx validation time p50 (ms)_ | 448.0 |
| _End-to-end TPS_ | 85.38 tx/s |
| _Backlog drain time (s)_ | 0.7 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 2.85 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 53.3 |
| _P99_ | 267.0ms |
| _P95_ | 206.9ms |
| _P50_ | 28.9ms |
| _Tx validation time p50 (ms)_ | 3.4 |
| _End-to-end TPS_ | 37.45 tx/s |
| _Sustained TPS_ | 33.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 37.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 270.5 |
| _P99_ | 271.5ms |
| _P95_ | 271.5ms |
| _P50_ | 270.8ms |
| _Tx validation time p50 (ms)_ | 237.8 |
| _End-to-end TPS_ | 220.82 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 7.36 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 153.0 |
| _P99_ | 670.9ms |
| _P95_ | 494.3ms |
| _P50_ | 119.1ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 13.06 tx/s |
| _Sustained TPS_ | 11.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 13.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 103.0 |
| _P99_ | 104.0ms |
| _P95_ | 104.0ms |
| _P50_ | 102.8ms |
| _Tx validation time p50 (ms)_ | 74.2 |
| _End-to-end TPS_ | 576.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.21 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 62.5 |
| _P99_ | 280.7ms |
| _P95_ | 190.2ms |
| _P50_ | 17.9ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 31.82 tx/s |
| _Sustained TPS_ | 32.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 31.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 215.7 |
| _P99_ | 217.2ms |
| _P95_ | 217.1ms |
| _P50_ | 215.8ms |
| _Tx validation time p50 (ms)_ | 143.4 |
| _End-to-end TPS_ | 413.26 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 9.18 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 120.7 |
| _P99_ | 666.5ms |
| _P95_ | 430.1ms |
| _P50_ | 36.1ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 24.80 tx/s |
| _Sustained TPS_ | 27.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 16.81 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 343.4 |
| _P99_ | 345.3ms |
| _P95_ | 345.2ms |
| _P50_ | 343.9ms |
| _Tx validation time p50 (ms)_ | 109.2 |
| _End-to-end TPS_ | 260.59 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 5.79 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 115.5 |
| _P99_ | 427.2ms |
| _P95_ | 280.2ms |
| _P50_ | 86.6ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 24.78 tx/s |
| _Sustained TPS_ | 31.19 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 16.80 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 139.4 |
| _P99_ | 140.8ms |
| _P95_ | 140.7ms |
| _P50_ | 140.4ms |
| _Tx validation time p50 (ms)_ | 32.3 |
| _End-to-end TPS_ | 637.18 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.16 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 46.7 |
| _P99_ | 215.9ms |
| _P95_ | 172.5ms |
| _P50_ | 29.3ms |
| _Tx validation time p50 (ms)_ | 6.4 |
| _End-to-end TPS_ | 63.45 tx/s |
| _Sustained TPS_ | 69.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 43.00 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
