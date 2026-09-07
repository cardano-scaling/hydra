--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-07 13:27:34.011600201 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 936.29 | n/a | 31.3 | 31.8 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 226.08 | 227.27 | 4.4 | 4.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 898.81 | n/a | 32.6 | 33.1 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 181.09 | 183.42 | 5.5 | 6.5 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 979.99 | n/a | 29.7 | 30.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 187.76 | 188.34 | 5.3 | 6.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 790.36 | n/a | 74.2 | 75.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 146.32 | 146.64 | 13.5 | 15.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 803.10 | n/a | 72.8 | 74.4 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 109.38 | 106.00 | 18.1 | 23.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 790.62 | n/a | 74.0 | 74.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 113.06 | 107.94 | 17.5 | 21.7 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 732.62 | n/a | 119.9 | 122.6 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 118.11 | 118.23 | 25.2 | 32.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 600.56 | n/a | 144.9 | 149.4 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 90.75 | 89.88 | 32.8 | 40.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 601.67 | n/a | 146.9 | 148.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 96.17 | 93.24 | 31.0 | 37.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.3 |
| _P99_ | 31.8ms |
| _P95_ | 31.8ms |
| _P50_ | 31.5ms |
| _Tx validation time p50 (ms)_ | 11.7 |
| _End-to-end TPS_ | 936.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 5.4ms |
| _P95_ | 4.9ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 226.08 tx/s |
| _Sustained TPS_ | 227.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 226.08 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.6 |
| _P99_ | 33.2ms |
| _P95_ | 33.1ms |
| _P50_ | 32.8ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 898.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 59.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 129.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.5 |
| _P99_ | 6.9ms |
| _P95_ | 6.5ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 181.09 tx/s |
| _Sustained TPS_ | 183.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 181.09 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.7 |
| _P99_ | 30.4ms |
| _P95_ | 30.4ms |
| _P50_ | 29.9ms |
| _Tx validation time p50 (ms)_ | 10.0 |
| _End-to-end TPS_ | 979.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 65.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.3 |
| _P99_ | 6.7ms |
| _P95_ | 6.6ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 187.76 tx/s |
| _Sustained TPS_ | 188.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 187.76 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.2 |
| _P99_ | 75.7ms |
| _P95_ | 75.6ms |
| _P50_ | 74.7ms |
| _Tx validation time p50 (ms)_ | 22.6 |
| _End-to-end TPS_ | 790.36 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.35 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.5 |
| _P99_ | 17.5ms |
| _P95_ | 15.4ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 3.6 |
| _End-to-end TPS_ | 146.32 tx/s |
| _Sustained TPS_ | 146.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 146.32 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 72.8 |
| _P99_ | 74.4ms |
| _P95_ | 74.4ms |
| _P50_ | 73.1ms |
| _Tx validation time p50 (ms)_ | 23.7 |
| _End-to-end TPS_ | 803.10 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.1 |
| _P99_ | 24.3ms |
| _P95_ | 23.8ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 109.38 tx/s |
| _Sustained TPS_ | 106.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 109.38 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 74.0 |
| _P99_ | 75.3ms |
| _P95_ | 74.9ms |
| _P50_ | 74.4ms |
| _Tx validation time p50 (ms)_ | 27.3 |
| _End-to-end TPS_ | 790.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.35 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 23.7ms |
| _P95_ | 21.7ms |
| _P50_ | 17.7ms |
| _Tx validation time p50 (ms)_ | 6.1 |
| _End-to-end TPS_ | 113.06 tx/s |
| _Sustained TPS_ | 107.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 113.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 119.9 |
| _P99_ | 122.6ms |
| _P95_ | 122.6ms |
| _P50_ | 120.8ms |
| _Tx validation time p50 (ms)_ | 52.4 |
| _End-to-end TPS_ | 732.62 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 16.28 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 33.6ms |
| _P95_ | 32.0ms |
| _P50_ | 24.7ms |
| _Tx validation time p50 (ms)_ | 6.2 |
| _End-to-end TPS_ | 118.11 tx/s |
| _Sustained TPS_ | 118.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 80.05 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.9 |
| _P99_ | 149.5ms |
| _P95_ | 149.4ms |
| _P50_ | 145.0ms |
| _Tx validation time p50 (ms)_ | 63.1 |
| _End-to-end TPS_ | 600.56 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.35 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.8 |
| _P99_ | 44.8ms |
| _P95_ | 40.8ms |
| _P50_ | 32.9ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 90.75 tx/s |
| _Sustained TPS_ | 89.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 60.50 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 146.9 |
| _P99_ | 148.8ms |
| _P95_ | 148.7ms |
| _P50_ | 148.3ms |
| _Tx validation time p50 (ms)_ | 51.9 |
| _End-to-end TPS_ | 601.67 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.37 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.0 |
| _P99_ | 42.7ms |
| _P95_ | 37.8ms |
| _P50_ | 31.3ms |
| _Tx validation time p50 (ms)_ | 8.7 |
| _End-to-end TPS_ | 96.17 tx/s |
| _Sustained TPS_ | 93.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 64.12 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
