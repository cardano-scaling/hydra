--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-02 13:45:56.882411778 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 890.83 | n/a | 33.1 | 33.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 183.99 | 184.44 | 5.4 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 902.70 | n/a | 32.7 | 33.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 159.14 | 159.98 | 6.2 | 8.1 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 246.55 | n/a | 120.7 | 121.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 95.59 | 91.25 | 10.4 | 20.7 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 484.92 | n/a | 122.6 | 123.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 111.13 | 149.26 | 17.8 | 19.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 737.69 | n/a | 80.0 | 80.6 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 122.07 | 121.21 | 16.2 | 20.0 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1192.55 | n/a | 49.1 | 49.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 118.03 | 112.63 | 16.8 | 24.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 746.47 | n/a | 118.7 | 120.4 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 130.98 | 132.07 | 22.5 | 27.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 678.21 | n/a | 130.0 | 132.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 99.88 | 99.61 | 29.5 | 35.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 803.45 | n/a | 109.5 | 111.3 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.8 | 107.08 | 104.95 | 27.6 | 43.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 33.1 |
| _P99_ | 33.5ms |
| _P95_ | 33.5ms |
| _P50_ | 33.3ms |
| _Tx validation time p50 (ms)_ | 18.1 |
| _End-to-end TPS_ | 890.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.39 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 9.5ms |
| _P95_ | 5.9ms |
| _P50_ | 5.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 183.99 tx/s |
| _Sustained TPS_ | 184.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 33.2ms |
| _P95_ | 33.1ms |
| _P50_ | 32.9ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 902.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 60.18 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.2 |
| _P99_ | 10.5ms |
| _P95_ | 8.1ms |
| _P50_ | 6.1ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 159.14 tx/s |
| _Sustained TPS_ | 159.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 159.14 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 120.7 |
| _P99_ | 121.5ms |
| _P95_ | 121.5ms |
| _P50_ | 121.3ms |
| _Tx validation time p50 (ms)_ | 23.5 |
| _End-to-end TPS_ | 246.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.44 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 10.4 |
| _P99_ | 71.0ms |
| _P95_ | 20.7ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 95.59 tx/s |
| _Sustained TPS_ | 91.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 95.59 /s |
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
| _Avg. Confirmation Time (ms)_ | 122.6 |
| _P99_ | 123.6ms |
| _P95_ | 123.5ms |
| _P50_ | 122.9ms |
| _Tx validation time p50 (ms)_ | 39.9 |
| _End-to-end TPS_ | 484.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.16 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.8 |
| _P99_ | 132.2ms |
| _P95_ | 19.5ms |
| _P50_ | 13.1ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 111.13 tx/s |
| _Sustained TPS_ | 149.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 111.13 /s |
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
| _Avg. Confirmation Time (ms)_ | 80.0 |
| _P99_ | 80.9ms |
| _P95_ | 80.6ms |
| _P50_ | 80.3ms |
| _Tx validation time p50 (ms)_ | 18.9 |
| _End-to-end TPS_ | 737.69 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.59 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.2 |
| _P99_ | 21.8ms |
| _P95_ | 20.0ms |
| _P50_ | 15.9ms |
| _Tx validation time p50 (ms)_ | 5.1 |
| _End-to-end TPS_ | 122.07 tx/s |
| _Sustained TPS_ | 121.21 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 122.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 49.1 |
| _P99_ | 49.9ms |
| _P95_ | 49.7ms |
| _P50_ | 49.3ms |
| _Tx validation time p50 (ms)_ | 17.7 |
| _End-to-end TPS_ | 1192.55 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.8 |
| _P99_ | 26.7ms |
| _P95_ | 24.6ms |
| _P50_ | 16.3ms |
| _Tx validation time p50 (ms)_ | 4.7 |
| _End-to-end TPS_ | 118.03 tx/s |
| _Sustained TPS_ | 112.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 118.03 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 118.7 |
| _P99_ | 120.4ms |
| _P95_ | 120.4ms |
| _P50_ | 118.7ms |
| _Tx validation time p50 (ms)_ | 43.3 |
| _End-to-end TPS_ | 746.47 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.59 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 22.5 |
| _P99_ | 31.4ms |
| _P95_ | 27.2ms |
| _P50_ | 22.4ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 130.98 tx/s |
| _Sustained TPS_ | 132.07 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 88.77 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 130.0 |
| _P99_ | 132.2ms |
| _P95_ | 132.1ms |
| _P50_ | 131.4ms |
| _Tx validation time p50 (ms)_ | 46.6 |
| _End-to-end TPS_ | 678.21 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.07 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 39.4ms |
| _P95_ | 35.9ms |
| _P50_ | 28.6ms |
| _Tx validation time p50 (ms)_ | 8.6 |
| _End-to-end TPS_ | 99.88 tx/s |
| _Sustained TPS_ | 99.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 67.70 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 147.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 109.5 |
| _P99_ | 111.4ms |
| _P95_ | 111.3ms |
| _P50_ | 111.0ms |
| _Tx validation time p50 (ms)_ | 53.1 |
| _End-to-end TPS_ | 803.45 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.85 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.6 |
| _P99_ | 48.7ms |
| _P95_ | 43.0ms |
| _P50_ | 26.7ms |
| _Tx validation time p50 (ms)_ | 7.4 |
| _End-to-end TPS_ | 107.08 tx/s |
| _Sustained TPS_ | 104.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.57 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
