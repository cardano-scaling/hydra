--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-19 16:48:07.271337437 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 919.49 | n/a | 31.7 | 32.4 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 197.40 | 198.40 | 5.0 | 5.7 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 811.57 | n/a | 36.1 | 36.8 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 160.99 | 159.86 | 6.1 | 7.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 789.14 | n/a | 37.2 | 37.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 161.60 | 161.61 | 6.1 | 7.9 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 800.09 | n/a | 73.2 | 74.8 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 121.57 | 123.41 | 16.3 | 21.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 666.92 | n/a | 88.1 | 89.0 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.7 | 92.16 | 92.47 | 21.5 | 25.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 836.34 | n/a | 70.4 | 71.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 96.64 | 92.37 | 20.5 | 26.1 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 631.38 | n/a | 139.5 | 142.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 97.73 | 96.62 | 30.2 | 37.0 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 590.04 | n/a | 148.1 | 150.8 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 81.08 | 82.20 | 36.3 | 43.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 629.13 | n/a | 139.0 | 142.1 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 88.75 | 84.70 | 33.5 | 41.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.7 |
| _P99_ | 32.4ms |
| _P95_ | 32.4ms |
| _P50_ | 31.9ms |
| _Tx validation time p50 (ms)_ | 13.6 |
| _End-to-end TPS_ | 919.49 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.30 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.0 |
| _P99_ | 6.5ms |
| _P95_ | 5.7ms |
| _P50_ | 4.9ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 197.40 tx/s |
| _Sustained TPS_ | 198.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 197.40 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 36.1 |
| _P99_ | 36.8ms |
| _P95_ | 36.8ms |
| _P50_ | 36.4ms |
| _Tx validation time p50 (ms)_ | 11.7 |
| _End-to-end TPS_ | 811.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 54.10 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 7.5ms |
| _P95_ | 7.2ms |
| _P50_ | 6.1ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 160.99 tx/s |
| _Sustained TPS_ | 159.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 160.99 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 37.2 |
| _P99_ | 37.8ms |
| _P95_ | 37.8ms |
| _P50_ | 37.5ms |
| _Tx validation time p50 (ms)_ | 20.9 |
| _End-to-end TPS_ | 789.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 52.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.1 |
| _P99_ | 8.1ms |
| _P95_ | 7.9ms |
| _P50_ | 5.8ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 161.60 tx/s |
| _Sustained TPS_ | 161.61 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 161.60 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.2 |
| _P99_ | 74.9ms |
| _P95_ | 74.8ms |
| _P50_ | 73.1ms |
| _Tx validation time p50 (ms)_ | 26.2 |
| _End-to-end TPS_ | 800.09 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.67 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.3 |
| _P99_ | 22.7ms |
| _P95_ | 21.3ms |
| _P50_ | 15.8ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 121.57 tx/s |
| _Sustained TPS_ | 123.41 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 121.57 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 88.1 |
| _P99_ | 89.4ms |
| _P95_ | 89.0ms |
| _P50_ | 88.5ms |
| _Tx validation time p50 (ms)_ | 36.7 |
| _End-to-end TPS_ | 666.92 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.23 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.5 |
| _P99_ | 26.7ms |
| _P95_ | 25.3ms |
| _P50_ | 21.6ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 92.16 tx/s |
| _Sustained TPS_ | 92.47 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 92.16 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 173.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.4 |
| _P99_ | 71.6ms |
| _P95_ | 71.5ms |
| _P50_ | 70.7ms |
| _Tx validation time p50 (ms)_ | 25.8 |
| _End-to-end TPS_ | 836.34 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.88 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.5 |
| _P99_ | 31.4ms |
| _P95_ | 26.1ms |
| _P50_ | 20.3ms |
| _Tx validation time p50 (ms)_ | 7.0 |
| _End-to-end TPS_ | 96.64 tx/s |
| _Sustained TPS_ | 92.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.64 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 139.5 |
| _P99_ | 142.3ms |
| _P95_ | 142.2ms |
| _P50_ | 140.5ms |
| _Tx validation time p50 (ms)_ | 81.5 |
| _End-to-end TPS_ | 631.38 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.03 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 30.2 |
| _P99_ | 39.0ms |
| _P95_ | 37.0ms |
| _P50_ | 30.5ms |
| _Tx validation time p50 (ms)_ | 8.0 |
| _End-to-end TPS_ | 97.73 tx/s |
| _Sustained TPS_ | 96.62 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 66.24 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 148.1 |
| _P99_ | 150.9ms |
| _P95_ | 150.8ms |
| _P50_ | 148.6ms |
| _Tx validation time p50 (ms)_ | 52.9 |
| _End-to-end TPS_ | 590.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.11 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 173.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.3 |
| _P99_ | 44.7ms |
| _P95_ | 43.0ms |
| _P50_ | 36.9ms |
| _Tx validation time p50 (ms)_ | 10.9 |
| _End-to-end TPS_ | 81.08 tx/s |
| _Sustained TPS_ | 82.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.95 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 139.0 |
| _P99_ | 142.3ms |
| _P95_ | 142.1ms |
| _P50_ | 139.5ms |
| _Tx validation time p50 (ms)_ | 59.3 |
| _End-to-end TPS_ | 629.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.98 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 33.5 |
| _P99_ | 49.6ms |
| _P95_ | 41.1ms |
| _P50_ | 34.0ms |
| _Tx validation time p50 (ms)_ | 10.1 |
| _End-to-end TPS_ | 88.75 tx/s |
| _Sustained TPS_ | 84.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 60.15 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
