--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-09 12:55:12.114316662 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1056.91 | n/a | 27.8 | 28.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 220.66 | 221.69 | 4.5 | 4.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 895.00 | n/a | 32.8 | 33.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 171.49 | 171.87 | 5.8 | 6.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 984.10 | n/a | 29.9 | 30.3 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 200.10 | 197.48 | 4.9 | 5.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 963.25 | n/a | 60.8 | 61.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 138.78 | 136.11 | 14.2 | 19.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 730.92 | n/a | 80.4 | 81.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 104.14 | 103.52 | 19.0 | 24.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 966.79 | n/a | 60.5 | 61.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 96.67 | 92.28 | 20.4 | 30.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 579.54 | n/a | 151.6 | 155.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 104.96 | 103.63 | 28.0 | 36.1 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 572.16 | n/a | 154.4 | 156.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 84.30 | 86.77 | 34.8 | 41.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 590.93 | n/a | 147.0 | 150.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 80.29 | 79.54 | 36.8 | 48.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.8 |
| _P99_ | 28.3ms |
| _P95_ | 28.2ms |
| _P50_ | 28.1ms |
| _Tx validation time p50 (ms)_ | 13.6 |
| _End-to-end TPS_ | 1056.91 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.46 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 5.3ms |
| _P95_ | 4.9ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 220.66 tx/s |
| _Sustained TPS_ | 221.69 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 220.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 33.4ms |
| _P95_ | 33.3ms |
| _P50_ | 33.1ms |
| _Tx validation time p50 (ms)_ | 15.0 |
| _End-to-end TPS_ | 895.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.67 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.1ms |
| _P95_ | 6.4ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 171.49 tx/s |
| _Sustained TPS_ | 171.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 171.49 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 130.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.9 |
| _P99_ | 30.3ms |
| _P95_ | 30.3ms |
| _P50_ | 30.1ms |
| _Tx validation time p50 (ms)_ | 10.5 |
| _End-to-end TPS_ | 984.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.9 |
| _P99_ | 6.4ms |
| _P95_ | 5.5ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 200.10 tx/s |
| _Sustained TPS_ | 197.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 200.10 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 60.8 |
| _P99_ | 61.9ms |
| _P95_ | 61.6ms |
| _P50_ | 61.2ms |
| _Tx validation time p50 (ms)_ | 21.1 |
| _End-to-end TPS_ | 963.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.11 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 14.2 |
| _P99_ | 22.9ms |
| _P95_ | 19.8ms |
| _P50_ | 13.2ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 138.78 tx/s |
| _Sustained TPS_ | 136.11 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 138.78 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.4 |
| _P99_ | 81.9ms |
| _P95_ | 81.9ms |
| _P50_ | 80.7ms |
| _Tx validation time p50 (ms)_ | 23.9 |
| _End-to-end TPS_ | 730.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.36 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 26.0ms |
| _P95_ | 24.8ms |
| _P50_ | 19.0ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 104.14 tx/s |
| _Sustained TPS_ | 103.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 104.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 60.5 |
| _P99_ | 61.6ms |
| _P95_ | 61.2ms |
| _P50_ | 60.8ms |
| _Tx validation time p50 (ms)_ | 24.1 |
| _End-to-end TPS_ | 966.79 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.23 /s |
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
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 35.0ms |
| _P95_ | 30.5ms |
| _P50_ | 19.3ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 96.67 tx/s |
| _Sustained TPS_ | 92.28 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.67 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 151.6 |
| _P99_ | 155.1ms |
| _P95_ | 155.0ms |
| _P50_ | 153.2ms |
| _Tx validation time p50 (ms)_ | 50.2 |
| _End-to-end TPS_ | 579.54 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.88 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.0 |
| _P99_ | 39.5ms |
| _P95_ | 36.1ms |
| _P50_ | 27.4ms |
| _Tx validation time p50 (ms)_ | 7.2 |
| _End-to-end TPS_ | 104.96 tx/s |
| _Sustained TPS_ | 103.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 71.14 /s |
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
| _Avg. Confirmation Time (ms)_ | 154.4 |
| _P99_ | 156.6ms |
| _P95_ | 156.5ms |
| _P50_ | 154.7ms |
| _Tx validation time p50 (ms)_ | 59.3 |
| _End-to-end TPS_ | 572.16 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.71 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 45.2ms |
| _P95_ | 41.9ms |
| _P50_ | 35.0ms |
| _Tx validation time p50 (ms)_ | 9.5 |
| _End-to-end TPS_ | 84.30 tx/s |
| _Sustained TPS_ | 86.77 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 57.14 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 147.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 147.0 |
| _P99_ | 150.3ms |
| _P95_ | 150.2ms |
| _P50_ | 148.6ms |
| _Tx validation time p50 (ms)_ | 58.5 |
| _End-to-end TPS_ | 590.93 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.13 /s |
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
| _Avg. Confirmation Time (ms)_ | 36.8 |
| _P99_ | 48.9ms |
| _P95_ | 48.0ms |
| _P50_ | 36.1ms |
| _Tx validation time p50 (ms)_ | 11.4 |
| _End-to-end TPS_ | 80.29 tx/s |
| _Sustained TPS_ | 79.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.42 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
