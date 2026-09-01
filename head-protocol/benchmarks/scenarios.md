--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-09-01 16:50:12.454693169 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1537.13 | n/a | 19.0 | 19.3 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.4 | 85.06 | 86.11 | 11.7 | 31.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.2 | 185.69 | n/a | 160.9 | 161.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 207.17 | 215.18 | 4.8 | 5.8 |
| Nodes=1, Mixed, fire and forget | 30 | 0.1 | 566.71 | n/a | 51.2 | 52.7 |
| Nodes=1, Mixed, wait for tx valid | 30 | 1.2 | 24.49 | 21.92 | 40.8 | 80.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.1 | 979.98 | n/a | 59.7 | 60.9 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.3 | 176.46 | 174.63 | 11.2 | 16.3 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1174.06 | n/a | 49.8 | 50.3 |
| Nodes=2, Growing, wait for tx valid | 60 | 1.0 | 60.91 | 76.46 | 32.7 | 151.3 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1290.37 | n/a | 45.4 | 46.2 |
| Nodes=2, Mixed, wait for tx valid | 60 | 2.1 | 28.13 | 24.80 | 70.9 | 274.4 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 900.91 | n/a | 97.0 | 98.0 |
| Nodes=3, Constant, wait for tx valid | 90 | 2.8 | 31.63 | 28.50 | 94.3 | 235.6 |
| Nodes=3, Growing, fire and forget | 90 | 0.2 | 534.08 | n/a | 165.9 | 168.3 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.9 | 102.89 | 103.96 | 28.3 | 56.2 |
| Nodes=3, Mixed, fire and forget | 90 | 0.3 | 337.19 | n/a | 264.8 | 266.7 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.9 | 103.47 | 114.08 | 28.8 | 34.5 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 19.0 |
| _P99_ | 19.3ms |
| _P95_ | 19.3ms |
| _P50_ | 19.1ms |
| _Tx validation time p50 (ms)_ | 9.0 |
| _End-to-end TPS_ | 1537.13 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 102.48 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.7 |
| _P99_ | 149.7ms |
| _P95_ | 31.5ms |
| _P50_ | 3.8ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 85.06 tx/s |
| _Sustained TPS_ | 86.11 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 85.06 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 160.9 |
| _P99_ | 161.4ms |
| _P95_ | 161.3ms |
| _P50_ | 161.0ms |
| _Tx validation time p50 (ms)_ | 53.8 |
| _End-to-end TPS_ | 185.69 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 12.38 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 131.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.8 |
| _P99_ | 9.2ms |
| _P95_ | 5.8ms |
| _P50_ | 4.6ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 207.17 tx/s |
| _Sustained TPS_ | 215.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 207.17 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 131.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 51.2 |
| _P99_ | 52.7ms |
| _P95_ | 52.7ms |
| _P50_ | 52.4ms |
| _Tx validation time p50 (ms)_ | 11.9 |
| _End-to-end TPS_ | 566.71 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 37.78 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 143.8 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 40.8 |
| _P99_ | 113.8ms |
| _P95_ | 80.5ms |
| _P50_ | 35.6ms |
| _Tx validation time p50 (ms)_ | 1.8 |
| _End-to-end TPS_ | 24.49 tx/s |
| _Sustained TPS_ | 21.92 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 24.49 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 128.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 59.7 |
| _P99_ | 61.0ms |
| _P95_ | 60.9ms |
| _P50_ | 59.8ms |
| _Tx validation time p50 (ms)_ | 33.9 |
| _End-to-end TPS_ | 979.98 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 32.67 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 145.1 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 11.2 |
| _P99_ | 37.1ms |
| _P95_ | 16.3ms |
| _P50_ | 10.0ms |
| _Tx validation time p50 (ms)_ | 2.9 |
| _End-to-end TPS_ | 176.46 tx/s |
| _Sustained TPS_ | 174.63 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 176.46 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.3 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 49.8 |
| _P99_ | 50.6ms |
| _P95_ | 50.3ms |
| _P50_ | 50.0ms |
| _Tx validation time p50 (ms)_ | 21.2 |
| _End-to-end TPS_ | 1174.06 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.14 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 143.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 32.7 |
| _P99_ | 199.0ms |
| _P95_ | 151.3ms |
| _P50_ | 15.1ms |
| _Tx validation time p50 (ms)_ | 5.3 |
| _End-to-end TPS_ | 60.91 tx/s |
| _Sustained TPS_ | 76.46 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 60.91 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 144.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 45.4 |
| _P99_ | 46.2ms |
| _P95_ | 46.2ms |
| _P50_ | 45.5ms |
| _Tx validation time p50 (ms)_ | 19.8 |
| _End-to-end TPS_ | 1290.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 43.01 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 144.4 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 70.9 |
| _P99_ | 450.1ms |
| _P95_ | 274.4ms |
| _P50_ | 14.6ms |
| _Tx validation time p50 (ms)_ | 4.2 |
| _End-to-end TPS_ | 28.13 tx/s |
| _Sustained TPS_ | 24.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 28.13 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 145.7 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 97.0 |
| _P99_ | 98.3ms |
| _P95_ | 98.0ms |
| _P50_ | 97.4ms |
| _Tx validation time p50 (ms)_ | 39.4 |
| _End-to-end TPS_ | 900.91 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.02 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 94.3 |
| _P99_ | 458.4ms |
| _P95_ | 235.6ms |
| _P50_ | 20.2ms |
| _Tx validation time p50 (ms)_ | 5.2 |
| _End-to-end TPS_ | 31.63 tx/s |
| _Sustained TPS_ | 28.50 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 62 |
| _Snapshots per second_ | 21.79 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 144.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 165.9 |
| _P99_ | 168.4ms |
| _P95_ | 168.3ms |
| _P50_ | 165.9ms |
| _Tx validation time p50 (ms)_ | 87.4 |
| _End-to-end TPS_ | 534.08 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.87 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 145.2 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.3 |
| _P99_ | 64.5ms |
| _P95_ | 56.2ms |
| _P50_ | 26.7ms |
| _Tx validation time p50 (ms)_ | 7.6 |
| _End-to-end TPS_ | 102.89 tx/s |
| _Sustained TPS_ | 103.96 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 69.73 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 146.0 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 264.8 |
| _P99_ | 266.7ms |
| _P95_ | 266.7ms |
| _P50_ | 264.7ms |
| _Tx validation time p50 (ms)_ | 210.4 |
| _End-to-end TPS_ | 337.19 tx/s |
| _Backlog drain time (s)_ | 0.3 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 7.49 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 144.9 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 28.8 |
| _P99_ | 141.6ms |
| _P95_ | 34.5ms |
| _P50_ | 25.5ms |
| _Tx validation time p50 (ms)_ | 6.9 |
| _End-to-end TPS_ | 103.47 tx/s |
| _Sustained TPS_ | 114.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 68.98 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 145.5 |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 4 |
      
