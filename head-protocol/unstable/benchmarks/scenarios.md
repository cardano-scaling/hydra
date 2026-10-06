--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-06 14:01:13.055748789 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1346.42 | n/a | 21.8 | 22.2 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 154.55 | 147.05 | 6.4 | 15.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1078.86 | n/a | 27.3 | 27.6 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 169.11 | 169.37 | 5.9 | 8.3 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1166.24 | n/a | 25.2 | 25.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 177.45 | 176.60 | 5.6 | 6.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1272.89 | n/a | 45.8 | 46.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 173.00 | 173.12 | 11.4 | 16.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1006.15 | n/a | 58.4 | 59.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 131.13 | 130.10 | 15.1 | 19.8 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1151.32 | n/a | 50.9 | 52.0 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.4 | 145.87 | 141.52 | 13.6 | 16.9 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1064.66 | n/a | 82.9 | 84.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.6 | 150.09 | 149.56 | 19.6 | 27.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 898.84 | n/a | 97.9 | 99.5 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.7 | 122.78 | 122.51 | 23.9 | 31.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 983.67 | n/a | 90.0 | 91.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.7 | 125.97 | 126.30 | 23.4 | 30.4 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 21.8 |
| _P99_ | 22.2ms |
| _P95_ | 22.2ms |
| _P50_ | 22.0ms |
| _Tx validation time p50 (ms)_ | 8.6 |
| _End-to-end TPS_ | 1346.42 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 89.76 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.4 |
| _P99_ | 26.5ms |
| _P95_ | 15.5ms |
| _P50_ | 5.0ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 154.55 tx/s |
| _Sustained TPS_ | 147.05 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 154.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 127.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.3 |
| _P99_ | 27.7ms |
| _P95_ | 27.6ms |
| _P50_ | 27.5ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 1078.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 71.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 132.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.9 |
| _P99_ | 8.4ms |
| _P95_ | 8.3ms |
| _P50_ | 5.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 169.11 tx/s |
| _Sustained TPS_ | 169.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.11 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.2 |
| _P99_ | 25.6ms |
| _P95_ | 25.5ms |
| _P50_ | 25.4ms |
| _Tx validation time p50 (ms)_ | 7.1 |
| _End-to-end TPS_ | 1166.24 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 77.75 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.2 |
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
| _P99_ | 7.0ms |
| _P95_ | 6.4ms |
| _P50_ | 5.5ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 177.45 tx/s |
| _Sustained TPS_ | 176.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 177.45 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 45.8 |
| _P99_ | 47.0ms |
| _P95_ | 46.9ms |
| _P50_ | 45.8ms |
| _Tx validation time p50 (ms)_ | 14.6 |
| _End-to-end TPS_ | 1272.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 42.43 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.4 |
| _P99_ | 17.4ms |
| _P95_ | 16.5ms |
| _P50_ | 11.0ms |
| _Tx validation time p50 (ms)_ | 2.7 |
| _End-to-end TPS_ | 173.00 tx/s |
| _Sustained TPS_ | 173.12 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 173.00 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 58.4 |
| _P99_ | 59.3ms |
| _P95_ | 59.0ms |
| _P50_ | 58.7ms |
| _Tx validation time p50 (ms)_ | 18.9 |
| _End-to-end TPS_ | 1006.15 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 33.54 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 146.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.1 |
| _P99_ | 20.2ms |
| _P95_ | 19.8ms |
| _P50_ | 14.6ms |
| _Tx validation time p50 (ms)_ | 5.0 |
| _End-to-end TPS_ | 131.13 tx/s |
| _Sustained TPS_ | 130.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 131.13 /s |
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
| _Avg. Confirmation Time (ms)_ | 50.9 |
| _P99_ | 52.0ms |
| _P95_ | 52.0ms |
| _P50_ | 50.6ms |
| _Tx validation time p50 (ms)_ | 12.2 |
| _End-to-end TPS_ | 1151.32 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 38.38 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.6 |
| _P99_ | 17.3ms |
| _P95_ | 16.9ms |
| _P50_ | 13.5ms |
| _Tx validation time p50 (ms)_ | 3.4 |
| _End-to-end TPS_ | 145.87 tx/s |
| _Sustained TPS_ | 141.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 145.87 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 82.9 |
| _P99_ | 84.3ms |
| _P95_ | 84.3ms |
| _P50_ | 83.5ms |
| _Tx validation time p50 (ms)_ | 26.0 |
| _End-to-end TPS_ | 1064.66 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.66 /s |
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
| _Avg. Confirmation Time (ms)_ | 19.6 |
| _P99_ | 28.9ms |
| _P95_ | 27.8ms |
| _P50_ | 18.8ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 150.09 tx/s |
| _Sustained TPS_ | 149.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 101.73 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 97.9 |
| _P99_ | 99.6ms |
| _P95_ | 99.5ms |
| _P50_ | 99.2ms |
| _Tx validation time p50 (ms)_ | 31.6 |
| _End-to-end TPS_ | 898.84 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.97 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 146.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.9 |
| _P99_ | 37.4ms |
| _P95_ | 31.8ms |
| _P50_ | 23.5ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 122.78 tx/s |
| _Sustained TPS_ | 122.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 83.22 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 90.0 |
| _P99_ | 91.0ms |
| _P95_ | 91.0ms |
| _P50_ | 90.6ms |
| _Tx validation time p50 (ms)_ | 35.3 |
| _End-to-end TPS_ | 983.67 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.86 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.4 |
| _P99_ | 33.5ms |
| _P95_ | 30.4ms |
| _P50_ | 23.2ms |
| _Tx validation time p50 (ms)_ | 5.9 |
| _End-to-end TPS_ | 125.97 tx/s |
| _Sustained TPS_ | 126.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 85.38 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
