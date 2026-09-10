--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-10 13:31:54.126937716 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1101.93 | n/a | 26.5 | 27.0 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 192.09 | 191.65 | 5.1 | 7.1 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 727.81 | n/a | 40.4 | 41.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 155.24 | 155.14 | 6.3 | 7.6 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 939.15 | n/a | 31.1 | 31.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 169.70 | 165.94 | 5.8 | 7.3 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 927.40 | n/a | 63.3 | 63.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 123.17 | 123.68 | 16.0 | 23.8 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 672.14 | n/a | 86.4 | 88.9 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 100.58 | 99.40 | 19.6 | 23.4 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 951.75 | n/a | 61.4 | 62.1 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.6 | 103.93 | 98.46 | 18.9 | 25.5 |
| Nodes=3, Constant, fire and forget | 90 | 0.2 | 577.34 | n/a | 152.1 | 154.8 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.8 | 110.56 | 111.93 | 26.6 | 34.8 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 520.37 | n/a | 169.8 | 171.2 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 82.89 | 81.60 | 35.6 | 43.8 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 673.81 | n/a | 130.2 | 131.2 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 91.16 | 88.67 | 32.5 | 39.6 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 26.5 |
| _P99_ | 27.0ms |
| _P95_ | 27.0ms |
| _P50_ | 26.7ms |
| _Tx validation time p50 (ms)_ | 17.7 |
| _End-to-end TPS_ | 1101.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 73.46 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.1 |
| _P99_ | 10.4ms |
| _P95_ | 7.1ms |
| _P50_ | 4.8ms |
| _Tx validation time p50 (ms)_ | 1.7 |
| _End-to-end TPS_ | 192.09 tx/s |
| _Sustained TPS_ | 191.65 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 192.09 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.6 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 40.4 |
| _P99_ | 41.0ms |
| _P95_ | 41.0ms |
| _P50_ | 40.7ms |
| _Tx validation time p50 (ms)_ | 11.1 |
| _End-to-end TPS_ | 727.81 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 48.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 6.3 |
| _P99_ | 7.8ms |
| _P95_ | 7.6ms |
| _P50_ | 6.3ms |
| _Tx validation time p50 (ms)_ | 1.9 |
| _End-to-end TPS_ | 155.24 tx/s |
| _Sustained TPS_ | 155.14 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 155.24 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 142.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 31.7ms |
| _P95_ | 31.7ms |
| _P50_ | 31.4ms |
| _Tx validation time p50 (ms)_ | 13.4 |
| _End-to-end TPS_ | 939.15 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 62.61 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 8.3ms |
| _P95_ | 7.3ms |
| _P50_ | 5.7ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 169.70 tx/s |
| _Sustained TPS_ | 165.94 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 169.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 143.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 63.3 |
| _P99_ | 64.0ms |
| _P95_ | 63.9ms |
| _P50_ | 63.5ms |
| _Tx validation time p50 (ms)_ | 22.5 |
| _End-to-end TPS_ | 927.40 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 30.91 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 16.0 |
| _P99_ | 27.0ms |
| _P95_ | 23.8ms |
| _P50_ | 15.4ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 123.17 tx/s |
| _Sustained TPS_ | 123.68 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 123.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.4 |
| _P99_ | 89.0ms |
| _P95_ | 88.9ms |
| _P50_ | 86.4ms |
| _Tx validation time p50 (ms)_ | 23.6 |
| _End-to-end TPS_ | 672.14 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.40 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.6 |
| _P99_ | 25.9ms |
| _P95_ | 23.4ms |
| _P50_ | 19.5ms |
| _Tx validation time p50 (ms)_ | 5.8 |
| _End-to-end TPS_ | 100.58 tx/s |
| _Sustained TPS_ | 99.40 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 100.58 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 61.4 |
| _P99_ | 62.5ms |
| _P95_ | 62.1ms |
| _P50_ | 61.7ms |
| _Tx validation time p50 (ms)_ | 32.0 |
| _End-to-end TPS_ | 951.75 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 31.72 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 18.9 |
| _P99_ | 26.2ms |
| _P95_ | 25.5ms |
| _P50_ | 19.8ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 103.93 tx/s |
| _Sustained TPS_ | 98.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 103.93 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 152.1 |
| _P99_ | 155.0ms |
| _P95_ | 154.8ms |
| _P50_ | 152.7ms |
| _Tx validation time p50 (ms)_ | 45.6 |
| _End-to-end TPS_ | 577.34 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.83 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 26.6 |
| _P99_ | 39.9ms |
| _P95_ | 34.8ms |
| _P50_ | 26.1ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 110.56 tx/s |
| _Sustained TPS_ | 111.93 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 74.93 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 169.8 |
| _P99_ | 171.2ms |
| _P95_ | 171.2ms |
| _P50_ | 170.4ms |
| _Tx validation time p50 (ms)_ | 57.0 |
| _End-to-end TPS_ | 520.37 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.56 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 35.6 |
| _P99_ | 45.7ms |
| _P95_ | 43.8ms |
| _P50_ | 36.0ms |
| _Tx validation time p50 (ms)_ | 10.7 |
| _End-to-end TPS_ | 82.89 tx/s |
| _Sustained TPS_ | 81.60 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 56.18 /s |
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
| _Avg. Confirmation Time (ms)_ | 130.2 |
| _P99_ | 131.3ms |
| _P95_ | 131.2ms |
| _P50_ | 130.5ms |
| _Tx validation time p50 (ms)_ | 68.4 |
| _End-to-end TPS_ | 673.81 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 14.97 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.5 |
| _P99_ | 46.8ms |
| _P95_ | 39.6ms |
| _P50_ | 31.8ms |
| _Tx validation time p50 (ms)_ | 8.9 |
| _End-to-end TPS_ | 91.16 tx/s |
| _Sustained TPS_ | 88.67 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 61.78 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
