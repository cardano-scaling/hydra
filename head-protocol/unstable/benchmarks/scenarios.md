--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-08-26 21:41:12.075559626 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1179.99 | n/a | 25.0 | 25.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 241.33 | 244.22 | 4.1 | 5.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 995.00 | n/a | 29.5 | 29.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 193.60 | 191.29 | 5.1 | 6.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 430.66 | n/a | 67.8 | 69.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 98.34 | 208.23 | 10.1 | 7.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1218.42 | n/a | 48.0 | 49.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 158.07 | 155.72 | 12.5 | 16.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1075.19 | n/a | 54.2 | 55.5 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 128.97 | 131.09 | 15.3 | 21.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1186.40 | n/a | 47.8 | 50.3 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.7 | 83.53 | 76.01 | 23.8 | 146.2 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 569.46 | n/a | 154.8 | 156.6 |
| Nodes=3, Constant, wait for tx valid | 90 | 1.0 | 85.74 | 80.17 | 34.8 | 104.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 793.92 | n/a | 110.7 | 113.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.2 | 73.28 | 78.76 | 40.3 | 105.9 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 771.02 | n/a | 114.8 | 116.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.1 | 83.01 | 80.24 | 35.8 | 42.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 25.3ms |
| _P95_ | 25.3ms |
| _P50_ | 25.1ms |
| _Tx validation time p50 (ms)_ | 14.2 |
| _End-to-end TPS_ | 1179.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 78.67 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.1 |
| _P99_ | 5.2ms |
| _P95_ | 5.0ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 241.33 tx/s |
| _Sustained TPS_ | 244.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 241.33 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 29.9ms |
| _P95_ | 29.9ms |
| _P50_ | 29.7ms |
| _Tx validation time p50 (ms)_ | 16.2 |
| _End-to-end TPS_ | 995.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 66.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 7.8ms |
| _P95_ | 6.6ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 193.60 tx/s |
| _Sustained TPS_ | 191.29 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 193.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 67.8 |
| _P99_ | 69.5ms |
| _P95_ | 69.4ms |
| _P50_ | 69.2ms |
| _Tx validation time p50 (ms)_ | 11.9 |
| _End-to-end TPS_ | 430.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.71 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 10.1 |
| _P99_ | 121.6ms |
| _P95_ | 7.2ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 98.34 tx/s |
| _Sustained TPS_ | 208.23 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 98.34 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 48.0 |
| _P99_ | 49.1ms |
| _P95_ | 49.0ms |
| _P50_ | 48.1ms |
| _Tx validation time p50 (ms)_ | 17.8 |
| _End-to-end TPS_ | 1218.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 40.61 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 12.5 |
| _P99_ | 18.1ms |
| _P95_ | 16.5ms |
| _P50_ | 12.2ms |
| _Tx validation time p50 (ms)_ | 3.2 |
| _End-to-end TPS_ | 158.07 tx/s |
| _Sustained TPS_ | 155.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 158.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 54.2 |
| _P99_ | 55.5ms |
| _P95_ | 55.5ms |
| _P50_ | 54.4ms |
| _Tx validation time p50 (ms)_ | 16.0 |
| _End-to-end TPS_ | 1075.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 35.84 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.3 |
| _P99_ | 28.1ms |
| _P95_ | 21.5ms |
| _P50_ | 14.5ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 128.97 tx/s |
| _Sustained TPS_ | 131.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 128.97 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 47.8 |
| _P99_ | 50.4ms |
| _P95_ | 50.3ms |
| _P50_ | 46.9ms |
| _Tx validation time p50 (ms)_ | 28.3 |
| _End-to-end TPS_ | 1186.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.55 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.8 |
| _P99_ | 155.6ms |
| _P95_ | 146.2ms |
| _P50_ | 14.8ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 83.53 tx/s |
| _Sustained TPS_ | 76.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 83.53 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 154.8 |
| _P99_ | 156.8ms |
| _P95_ | 156.6ms |
| _P50_ | 156.2ms |
| _Tx validation time p50 (ms)_ | 30.2 |
| _End-to-end TPS_ | 569.46 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.65 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.8 |
| _P99_ | 121.0ms |
| _P95_ | 104.8ms |
| _P50_ | 21.4ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 85.74 tx/s |
| _Sustained TPS_ | 80.17 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 57.16 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 110.7 |
| _P99_ | 113.1ms |
| _P95_ | 113.0ms |
| _P50_ | 110.3ms |
| _Tx validation time p50 (ms)_ | 40.7 |
| _End-to-end TPS_ | 793.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.64 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 40.3 |
| _P99_ | 169.1ms |
| _P95_ | 105.9ms |
| _P50_ | 34.2ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 73.28 tx/s |
| _Sustained TPS_ | 78.76 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 49.67 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 114.8 |
| _P99_ | 116.6ms |
| _P95_ | 116.5ms |
| _P50_ | 114.6ms |
| _Tx validation time p50 (ms)_ | 36.4 |
| _End-to-end TPS_ | 771.02 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 17.13 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.8 |
| _P99_ | 277.6ms |
| _P95_ | 42.1ms |
| _P50_ | 26.3ms |
| _Tx validation time p50 (ms)_ | 7.6 |
| _End-to-end TPS_ | 83.01 tx/s |
| _Sustained TPS_ | 80.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.26 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
