--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-10-01 08:38:11.06803583 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.1 | 288.78 | n/a | 103.3 | 103.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 232.38 | 234.71 | 4.2 | 5.9 |
| Nodes=1, Growing, fire and forget | 30 | 0.2 | 174.94 | n/a | 170.8 | 171.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.3 | 115.05 | 186.52 | 8.6 | 7.7 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 424.77 | n/a | 68.5 | 70.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 213.56 | 210.31 | 4.6 | 5.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.2 | 377.53 | n/a | 157.4 | 158.7 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 150.15 | 152.54 | 13.2 | 25.2 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 805.84 | n/a | 73.2 | 74.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 127.07 | 127.63 | 15.6 | 19.7 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1107.04 | n/a | 52.7 | 53.5 |
| Nodes=2, Mixed, wait for tx valid | 60 | 1.9 | 31.19 | 33.16 | 64.0 | 409.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 631.31 | n/a | 140.4 | 142.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.6 | 144.97 | 148.10 | 20.4 | 25.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 673.87 | n/a | 131.3 | 133.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 99.76 | 99.13 | 29.5 | 36.3 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 715.29 | n/a | 123.9 | 124.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 94.60 | 89.38 | 31.2 | 59.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 103.3 |
| _P99_ | 103.7ms |
| _P95_ | 103.7ms |
| _P50_ | 103.5ms |
| _Tx validation time p50 (ms)_ | 88.6 |
| _End-to-end TPS_ | 288.78 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 19.25 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.2 |
| _P99_ | 6.5ms |
| _P95_ | 5.9ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 232.38 tx/s |
| _Sustained TPS_ | 234.71 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 232.38 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 129.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 170.8 |
| _P99_ | 171.3ms |
| _P95_ | 171.3ms |
| _P50_ | 171.1ms |
| _Tx validation time p50 (ms)_ | 156.3 |
| _End-to-end TPS_ | 174.94 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.66 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 8.6 |
| _P99_ | 76.8ms |
| _P95_ | 7.7ms |
| _P50_ | 5.2ms |
| _Tx validation time p50 (ms)_ | 1.5 |
| _End-to-end TPS_ | 115.05 tx/s |
| _Sustained TPS_ | 186.52 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 115.05 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 68.5 |
| _P99_ | 70.5ms |
| _P95_ | 70.4ms |
| _P50_ | 70.2ms |
| _Tx validation time p50 (ms)_ | 9.4 |
| _End-to-end TPS_ | 424.77 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 28.32 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 142.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 6.3ms |
| _P95_ | 5.3ms |
| _P50_ | 4.5ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 213.56 tx/s |
| _Sustained TPS_ | 210.31 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 213.56 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 157.4 |
| _P99_ | 158.8ms |
| _P95_ | 158.7ms |
| _P50_ | 157.8ms |
| _Tx validation time p50 (ms)_ | 123.4 |
| _End-to-end TPS_ | 377.53 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.58 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.2 |
| _P99_ | 26.5ms |
| _P95_ | 25.2ms |
| _P50_ | 11.9ms |
| _Tx validation time p50 (ms)_ | 3.0 |
| _End-to-end TPS_ | 150.15 tx/s |
| _Sustained TPS_ | 152.54 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 150.15 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 73.2 |
| _P99_ | 74.3ms |
| _P95_ | 74.2ms |
| _P50_ | 73.5ms |
| _Tx validation time p50 (ms)_ | 36.4 |
| _End-to-end TPS_ | 805.84 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.86 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 15.6 |
| _P99_ | 22.1ms |
| _P95_ | 19.7ms |
| _P50_ | 15.2ms |
| _Tx validation time p50 (ms)_ | 4.6 |
| _End-to-end TPS_ | 127.07 tx/s |
| _Sustained TPS_ | 127.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 127.07 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 52.7 |
| _P99_ | 53.8ms |
| _P95_ | 53.5ms |
| _P50_ | 52.9ms |
| _Tx validation time p50 (ms)_ | 20.3 |
| _End-to-end TPS_ | 1107.04 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 36.90 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 142.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 64.0 |
| _P99_ | 412.2ms |
| _P95_ | 409.5ms |
| _P50_ | 14.8ms |
| _Tx validation time p50 (ms)_ | 4.4 |
| _End-to-end TPS_ | 31.19 tx/s |
| _Sustained TPS_ | 33.16 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 31.19 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 140.4 |
| _P99_ | 142.4ms |
| _P95_ | 142.3ms |
| _P50_ | 140.8ms |
| _Tx validation time p50 (ms)_ | 59.5 |
| _End-to-end TPS_ | 631.31 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.03 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.4 |
| _P99_ | 28.1ms |
| _P95_ | 25.6ms |
| _P50_ | 20.0ms |
| _Tx validation time p50 (ms)_ | 4.9 |
| _End-to-end TPS_ | 144.97 tx/s |
| _Sustained TPS_ | 148.10 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 98.26 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 131.3 |
| _P99_ | 133.1ms |
| _P95_ | 133.0ms |
| _P50_ | 132.1ms |
| _Tx validation time p50 (ms)_ | 34.3 |
| _End-to-end TPS_ | 673.87 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.97 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.5 |
| _P99_ | 40.6ms |
| _P95_ | 36.3ms |
| _P50_ | 29.0ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 99.76 tx/s |
| _Sustained TPS_ | 99.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 67.61 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 123.9 |
| _P99_ | 124.9ms |
| _P95_ | 124.7ms |
| _P50_ | 124.3ms |
| _Tx validation time p50 (ms)_ | 58.8 |
| _End-to-end TPS_ | 715.29 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.90 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 31.2 |
| _P99_ | 112.9ms |
| _P95_ | 59.1ms |
| _P50_ | 26.8ms |
| _Tx validation time p50 (ms)_ | 8.2 |
| _End-to-end TPS_ | 94.60 tx/s |
| _Sustained TPS_ | 89.38 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 64.12 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 4 |
      
