--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-02 17:45:47.026396289 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1065.49 | n/a | 27.4 | 27.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 223.48 | 223.82 | 4.4 | 5.0 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 952.64 | n/a | 30.6 | 31.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 183.65 | 183.34 | 5.4 | 6.0 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 956.70 | n/a | 30.4 | 31.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 192.33 | 188.26 | 5.1 | 6.6 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 884.16 | n/a | 65.6 | 67.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 142.82 | 141.22 | 13.9 | 17.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 772.97 | n/a | 75.6 | 76.8 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 111.69 | 109.82 | 17.7 | 21.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 952.55 | n/a | 61.6 | 62.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.5 | 117.87 | 113.50 | 16.8 | 20.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 711.60 | n/a | 120.8 | 126.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 119.12 | 117.26 | 25.0 | 30.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 533.39 | n/a | 166.1 | 167.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.0 | 88.39 | 89.51 | 33.0 | 41.4 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 608.30 | n/a | 130.8 | 138.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 94.47 | 91.84 | 31.2 | 39.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.4 |
| _P99_ | 27.9ms |
| _P95_ | 27.9ms |
| _P50_ | 27.6ms |
| _Tx validation time p50 (ms)_ | 18.9 |
| _End-to-end TPS_ | 1065.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.03 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 127.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 5.9ms |
| _P95_ | 5.0ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.6 |
| _End-to-end TPS_ | 223.48 tx/s |
| _Sustained TPS_ | 223.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 223.48 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.6 |
| _P99_ | 31.3ms |
| _P95_ | 31.3ms |
| _P50_ | 30.8ms |
| _Tx validation time p50 (ms)_ | 10.3 |
| _End-to-end TPS_ | 952.64 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.51 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 6.3ms |
| _P95_ | 6.0ms |
| _P50_ | 5.4ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 183.65 tx/s |
| _Sustained TPS_ | 183.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 183.65 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 30.4 |
| _P99_ | 31.2ms |
| _P95_ | 31.1ms |
| _P50_ | 30.7ms |
| _Tx validation time p50 (ms)_ | 11.6 |
| _End-to-end TPS_ | 956.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 63.78 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 126.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 8.1ms |
| _P95_ | 6.6ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 192.33 tx/s |
| _Sustained TPS_ | 188.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.33 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 126.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 65.6 |
| _P99_ | 67.4ms |
| _P95_ | 67.0ms |
| _P50_ | 66.0ms |
| _Tx validation time p50 (ms)_ | 21.3 |
| _End-to-end TPS_ | 884.16 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 29.47 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 134.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.9 |
| _P99_ | 19.7ms |
| _P95_ | 17.4ms |
| _P50_ | 13.3ms |
| _Tx validation time p50 (ms)_ | 3.8 |
| _End-to-end TPS_ | 142.82 tx/s |
| _Sustained TPS_ | 141.22 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 142.82 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 75.6 |
| _P99_ | 76.9ms |
| _P95_ | 76.8ms |
| _P50_ | 75.9ms |
| _Tx validation time p50 (ms)_ | 21.5 |
| _End-to-end TPS_ | 772.97 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.77 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.7 |
| _P99_ | 22.1ms |
| _P95_ | 21.5ms |
| _P50_ | 17.8ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 111.69 tx/s |
| _Sustained TPS_ | 109.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 111.69 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.6 |
| _P99_ | 62.8ms |
| _P95_ | 62.7ms |
| _P50_ | 61.9ms |
| _Tx validation time p50 (ms)_ | 22.6 |
| _End-to-end TPS_ | 952.55 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.8 |
| _P99_ | 21.6ms |
| _P95_ | 20.6ms |
| _P50_ | 16.7ms |
| _Tx validation time p50 (ms)_ | 5.4 |
| _End-to-end TPS_ | 117.87 tx/s |
| _Sustained TPS_ | 113.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 117.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 120.8 |
| _P99_ | 126.4ms |
| _P95_ | 126.3ms |
| _P50_ | 120.2ms |
| _Tx validation time p50 (ms)_ | 64.8 |
| _End-to-end TPS_ | 711.60 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.81 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.0 |
| _P99_ | 33.6ms |
| _P95_ | 30.9ms |
| _P50_ | 24.7ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 119.12 tx/s |
| _Sustained TPS_ | 117.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 79.41 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 166.1 |
| _P99_ | 167.7ms |
| _P95_ | 167.6ms |
| _P50_ | 166.6ms |
| _Tx validation time p50 (ms)_ | 61.4 |
| _End-to-end TPS_ | 533.39 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.85 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.0 |
| _P99_ | 46.2ms |
| _P95_ | 41.4ms |
| _P50_ | 32.5ms |
| _Tx validation time p50 (ms)_ | 9.2 |
| _End-to-end TPS_ | 88.39 tx/s |
| _Sustained TPS_ | 89.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 60.89 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 130.8 |
| _P99_ | 138.8ms |
| _P95_ | 138.7ms |
| _P50_ | 129.8ms |
| _Tx validation time p50 (ms)_ | 52.2 |
| _End-to-end TPS_ | 608.30 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 3 |
| _Snapshots per second_ | 20.28 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.2 |
| _P99_ | 46.2ms |
| _P95_ | 39.6ms |
| _P50_ | 30.9ms |
| _Tx validation time p50 (ms)_ | 9.1 |
| _End-to-end TPS_ | 94.47 tx/s |
| _Sustained TPS_ | 91.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 65.08 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
