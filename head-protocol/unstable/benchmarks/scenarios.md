--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-06 14:15:55.769799686 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 915.72 | n/a | 32.1 | 32.5 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.2 | 140.70 | 143.78 | 7.0 | 9.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 853.78 | n/a | 34.4 | 34.9 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.2 | 129.63 | 130.03 | 7.6 | 9.2 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 849.44 | n/a | 34.7 | 35.1 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.2 | 139.91 | 138.83 | 7.1 | 8.2 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 836.25 | n/a | 70.2 | 71.5 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.5 | 114.54 | 116.36 | 17.3 | 21.5 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 726.65 | n/a | 80.5 | 82.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 94.66 | 93.96 | 20.9 | 25.9 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 793.14 | n/a | 73.9 | 74.7 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.7 | 91.31 | 88.84 | 21.6 | 28.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 693.21 | n/a | 126.9 | 129.2 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.9 | 100.10 | 100.87 | 29.1 | 34.9 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 609.19 | n/a | 144.4 | 146.6 |
| Nodes=3, Growing, wait for tx valid | 90 | 1.1 | 81.02 | 81.56 | 36.4 | 42.5 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 628.94 | n/a | 139.4 | 140.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 1.0 | 87.17 | 84.27 | 34.1 | 40.8 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 32.1 |
| _P99_ | 32.6ms |
| _P95_ | 32.5ms |
| _P50_ | 32.4ms |
| _Tx validation time p50 (ms)_ | 20.7 |
| _End-to-end TPS_ | 915.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 61.05 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.0 |
| _P99_ | 10.7ms |
| _P95_ | 9.5ms |
| _P50_ | 6.7ms |
| _Tx validation time p50 (ms)_ | 2.4 |
| _End-to-end TPS_ | 140.70 tx/s |
| _Sustained TPS_ | 143.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 140.70 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 170.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 34.4 |
| _P99_ | 35.0ms |
| _P95_ | 34.9ms |
| _P50_ | 34.8ms |
| _Tx validation time p50 (ms)_ | 13.1 |
| _End-to-end TPS_ | 853.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 56.92 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.6 |
| _P99_ | 10.0ms |
| _P95_ | 9.2ms |
| _P50_ | 7.4ms |
| _Tx validation time p50 (ms)_ | 2.3 |
| _End-to-end TPS_ | 129.63 tx/s |
| _Sustained TPS_ | 130.03 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 129.63 /s |
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
| _Avg. Confirmation Time (ms)_ | 34.7 |
| _P99_ | 35.1ms |
| _P95_ | 35.1ms |
| _P50_ | 34.9ms |
| _Tx validation time p50 (ms)_ | 12.5 |
| _End-to-end TPS_ | 849.44 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 56.63 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 7.1 |
| _P99_ | 9.2ms |
| _P95_ | 8.2ms |
| _P50_ | 6.8ms |
| _Tx validation time p50 (ms)_ | 2.3 |
| _End-to-end TPS_ | 139.91 tx/s |
| _Sustained TPS_ | 138.83 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 139.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 70.2 |
| _P99_ | 71.6ms |
| _P95_ | 71.5ms |
| _P50_ | 70.5ms |
| _Tx validation time p50 (ms)_ | 35.5 |
| _End-to-end TPS_ | 836.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 27.87 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 17.3 |
| _P99_ | 27.3ms |
| _P95_ | 21.5ms |
| _P50_ | 17.0ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 114.54 tx/s |
| _Sustained TPS_ | 116.36 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 114.54 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 80.5 |
| _P99_ | 82.4ms |
| _P95_ | 82.3ms |
| _P50_ | 80.9ms |
| _Tx validation time p50 (ms)_ | 23.7 |
| _End-to-end TPS_ | 726.65 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 24.22 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.9 |
| _P99_ | 36.1ms |
| _P95_ | 25.9ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 6.8 |
| _End-to-end TPS_ | 94.66 tx/s |
| _Sustained TPS_ | 93.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 94.66 /s |
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
| _Avg. Confirmation Time (ms)_ | 73.9 |
| _P99_ | 75.1ms |
| _P95_ | 74.7ms |
| _P50_ | 74.2ms |
| _Tx validation time p50 (ms)_ | 23.4 |
| _End-to-end TPS_ | 793.14 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 26.44 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 21.6 |
| _P99_ | 54.2ms |
| _P95_ | 28.8ms |
| _P50_ | 19.2ms |
| _Tx validation time p50 (ms)_ | 5.5 |
| _End-to-end TPS_ | 91.31 tx/s |
| _Sustained TPS_ | 88.84 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 91.31 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 126.9 |
| _P99_ | 129.3ms |
| _P95_ | 129.2ms |
| _P50_ | 126.6ms |
| _Tx validation time p50 (ms)_ | 42.7 |
| _End-to-end TPS_ | 693.21 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 15.40 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.1 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 29.1 |
| _P99_ | 38.4ms |
| _P95_ | 34.9ms |
| _P50_ | 29.2ms |
| _Tx validation time p50 (ms)_ | 7.9 |
| _End-to-end TPS_ | 100.10 tx/s |
| _Sustained TPS_ | 100.87 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 68.96 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 144.4 |
| _P99_ | 146.6ms |
| _P95_ | 146.6ms |
| _P50_ | 144.2ms |
| _Tx validation time p50 (ms)_ | 61.1 |
| _End-to-end TPS_ | 609.19 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.54 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 36.4 |
| _P99_ | 48.9ms |
| _P95_ | 42.5ms |
| _P50_ | 35.9ms |
| _Tx validation time p50 (ms)_ | 11.0 |
| _End-to-end TPS_ | 81.02 tx/s |
| _Sustained TPS_ | 81.56 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 54.91 /s |
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
| _Avg. Confirmation Time (ms)_ | 139.4 |
| _P99_ | 141.0ms |
| _P95_ | 140.7ms |
| _P50_ | 139.6ms |
| _Tx validation time p50 (ms)_ | 54.6 |
| _End-to-end TPS_ | 628.94 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 13.98 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 34.1 |
| _P99_ | 44.6ms |
| _P95_ | 40.8ms |
| _P50_ | 33.7ms |
| _Tx validation time p50 (ms)_ | 9.7 |
| _End-to-end TPS_ | 87.17 tx/s |
| _Sustained TPS_ | 84.27 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 58.11 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.2 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
