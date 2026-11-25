--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-25 18:28:52.47359386 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1144.01 | n/a | 25.7 | 26.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 261.08 | 260.08 | 3.8 | 4.8 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1054.88 | n/a | 27.7 | 28.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 223.96 | 221.40 | 4.4 | 5.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1452.99 | n/a | 20.1 | 20.5 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 230.22 | 225.01 | 4.3 | 6.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 767.64 | n/a | 76.8 | 77.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 172.62 | 172.89 | 11.5 | 14.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.0 | 1302.34 | n/a | 44.7 | 45.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 122.61 | 124.95 | 16.1 | 24.5 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1312.48 | n/a | 44.4 | 45.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.4 | 148.12 | 140.82 | 13.4 | 19.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 989.88 | n/a | 85.4 | 90.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 107.41 | 103.90 | 27.2 | 43.5 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 1019.68 | n/a | 85.9 | 87.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 108.82 | 108.13 | 27.0 | 34.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 1103.58 | n/a | 79.9 | 81.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.7 | 124.28 | 123.30 | 23.8 | 28.9 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 25.7 |
| _P99_ | 26.1ms |
| _P95_ | 26.0ms |
| _P50_ | 25.9ms |
| _Tx validation time p50 (ms)_ | 8.8 |
| _End-to-end TPS_ | 1144.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 76.27 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 3.8 |
| _P99_ | 7.4ms |
| _P95_ | 4.8ms |
| _P50_ | 3.5ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 261.08 tx/s |
| _Sustained TPS_ | 260.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 261.08 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 27.7 |
| _P99_ | 28.3ms |
| _P95_ | 28.3ms |
| _P50_ | 28.1ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 1054.88 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 70.33 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.4 |
| _P99_ | 6.9ms |
| _P95_ | 5.2ms |
| _P50_ | 4.3ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 223.96 tx/s |
| _Sustained TPS_ | 221.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 223.96 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 20.1 |
| _P99_ | 20.5ms |
| _P95_ | 20.5ms |
| _P50_ | 20.3ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 1452.99 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 96.87 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 8.0ms |
| _P95_ | 6.3ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 230.22 tx/s |
| _Sustained TPS_ | 225.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 230.22 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 173.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 76.8 |
| _P99_ | 78.0ms |
| _P95_ | 77.9ms |
| _P50_ | 77.0ms |
| _Tx validation time p50 (ms)_ | 47.5 |
| _End-to-end TPS_ | 767.64 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 25.59 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.5 |
| _P99_ | 16.9ms |
| _P95_ | 14.8ms |
| _P50_ | 11.5ms |
| _Tx validation time p50 (ms)_ | 2.8 |
| _End-to-end TPS_ | 172.62 tx/s |
| _Sustained TPS_ | 172.89 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 172.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 44.7 |
| _P99_ | 45.9ms |
| _P95_ | 45.9ms |
| _P50_ | 44.9ms |
| _Tx validation time p50 (ms)_ | 18.4 |
| _End-to-end TPS_ | 1302.34 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 43.41 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.1 |
| _P99_ | 33.5ms |
| _P95_ | 24.5ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 122.61 tx/s |
| _Sustained TPS_ | 124.95 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 122.61 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 44.4 |
| _P99_ | 45.6ms |
| _P95_ | 45.5ms |
| _P50_ | 44.6ms |
| _Tx validation time p50 (ms)_ | 15.1 |
| _End-to-end TPS_ | 1312.48 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 43.75 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.4 |
| _P99_ | 25.5ms |
| _P95_ | 19.1ms |
| _P50_ | 12.7ms |
| _Tx validation time p50 (ms)_ | 3.7 |
| _End-to-end TPS_ | 148.12 tx/s |
| _Sustained TPS_ | 140.82 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 148.12 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.4 |
| _P99_ | 90.3ms |
| _P95_ | 90.2ms |
| _P50_ | 86.1ms |
| _Tx validation time p50 (ms)_ | 46.1 |
| _End-to-end TPS_ | 989.88 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.00 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.2 |
| _P99_ | 46.3ms |
| _P95_ | 43.5ms |
| _P50_ | 25.7ms |
| _Tx validation time p50 (ms)_ | 6.5 |
| _End-to-end TPS_ | 107.41 tx/s |
| _Sustained TPS_ | 103.90 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 73.99 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 85.9 |
| _P99_ | 87.3ms |
| _P95_ | 87.1ms |
| _P50_ | 86.6ms |
| _Tx validation time p50 (ms)_ | 28.2 |
| _End-to-end TPS_ | 1019.68 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.66 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.0 |
| _P99_ | 36.6ms |
| _P95_ | 34.0ms |
| _P50_ | 27.2ms |
| _Tx validation time p50 (ms)_ | 7.7 |
| _End-to-end TPS_ | 108.82 tx/s |
| _Sustained TPS_ | 108.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 73.76 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 79.9 |
| _P99_ | 81.2ms |
| _P95_ | 81.2ms |
| _P50_ | 80.0ms |
| _Tx validation time p50 (ms)_ | 26.7 |
| _End-to-end TPS_ | 1103.58 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.52 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.8 |
| _P99_ | 33.5ms |
| _P95_ | 28.9ms |
| _P50_ | 23.6ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 124.28 tx/s |
| _Sustained TPS_ | 123.30 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 84.23 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
