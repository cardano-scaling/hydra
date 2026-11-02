--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-02 15:13:30.488999873 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1587.45 | n/a | 18.4 | 18.7 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 270.89 | 272.26 | 3.6 | 4.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1417.85 | n/a | 20.6 | 21.0 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 237.55 | 238.25 | 4.2 | 4.9 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1616.70 | n/a | 18.0 | 18.4 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.3 | 94.05 | 81.74 | 10.6 | 41.4 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1327.20 | n/a | 43.7 | 45.0 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.4 | 142.66 | 140.98 | 13.8 | 17.4 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1090.90 | n/a | 53.4 | 54.1 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.6 | 102.62 | 96.86 | 19.2 | 34.6 |
| Nodes=2, Mixed, fire and forget | 60 | 0.0 | 1344.35 | n/a | 43.3 | 43.9 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.4 | 144.85 | 140.26 | 13.7 | 18.6 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 1077.13 | n/a | 81.6 | 83.1 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 129.27 | 129.08 | 23.0 | 29.3 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 919.27 | n/a | 95.5 | 97.1 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 107.04 | 106.66 | 27.5 | 33.0 |
| Nodes=3, Mixed, fire and forget | 90 | 0.2 | 516.25 | n/a | 171.3 | 172.5 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.8 | 115.56 | 113.80 | 25.6 | 33.0 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 18.4 |
| _P99_ | 18.7ms |
| _P95_ | 18.7ms |
| _P50_ | 18.5ms |
| _Tx validation time p50 (ms)_ | 9.6 |
| _End-to-end TPS_ | 1587.45 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 105.83 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 3.6 |
| _P99_ | 4.7ms |
| _P95_ | 4.5ms |
| _P50_ | 3.5ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 270.89 tx/s |
| _Sustained TPS_ | 272.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 270.89 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 20.6 |
| _P99_ | 21.0ms |
| _P95_ | 21.0ms |
| _P50_ | 20.8ms |
| _Tx validation time p50 (ms)_ | 8.0 |
| _End-to-end TPS_ | 1417.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 94.52 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.2 |
| _P99_ | 5.2ms |
| _P95_ | 4.9ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 237.55 tx/s |
| _Sustained TPS_ | 238.25 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 237.55 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 18.0 |
| _P99_ | 18.4ms |
| _P95_ | 18.4ms |
| _P50_ | 18.1ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 1616.70 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 107.78 /s |
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
| _Avg. Confirmation Time (ms)_ | 10.6 |
| _P99_ | 68.2ms |
| _P95_ | 41.4ms |
| _P50_ | 4.1ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 94.05 tx/s |
| _Sustained TPS_ | 81.74 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 94.05 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.7 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 43.7 |
| _P99_ | 45.0ms |
| _P95_ | 45.0ms |
| _P50_ | 43.6ms |
| _Tx validation time p50 (ms)_ | 18.7 |
| _End-to-end TPS_ | 1327.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 44.24 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.8 |
| _P99_ | 89.9ms |
| _P95_ | 17.4ms |
| _P50_ | 10.7ms |
| _Tx validation time p50 (ms)_ | 3.3 |
| _End-to-end TPS_ | 142.66 tx/s |
| _Sustained TPS_ | 140.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 142.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Growing, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 53.4 |
| _P99_ | 54.2ms |
| _P95_ | 54.1ms |
| _P50_ | 53.7ms |
| _Tx validation time p50 (ms)_ | 22.0 |
| _End-to-end TPS_ | 1090.90 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 36.36 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Growing, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 100.2ms |
| _P95_ | 34.6ms |
| _P50_ | 14.8ms |
| _Tx validation time p50 (ms)_ | 4.5 |
| _End-to-end TPS_ | 102.62 tx/s |
| _Sustained TPS_ | 96.86 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 102.62 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 43.3 |
| _P99_ | 44.2ms |
| _P95_ | 43.9ms |
| _P50_ | 43.5ms |
| _Tx validation time p50 (ms)_ | 23.9 |
| _End-to-end TPS_ | 1344.35 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 44.81 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 13.7 |
| _P99_ | 19.1ms |
| _P95_ | 18.6ms |
| _P50_ | 13.6ms |
| _Tx validation time p50 (ms)_ | 4.3 |
| _End-to-end TPS_ | 144.85 tx/s |
| _Sustained TPS_ | 140.26 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 144.85 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.6 |
| _P99_ | 83.2ms |
| _P95_ | 83.1ms |
| _P50_ | 82.6ms |
| _Tx validation time p50 (ms)_ | 31.8 |
| _End-to-end TPS_ | 1077.13 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.94 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Constant, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 23.0 |
| _P99_ | 31.8ms |
| _P95_ | 29.3ms |
| _P50_ | 22.9ms |
| _Tx validation time p50 (ms)_ | 6.3 |
| _End-to-end TPS_ | 129.27 tx/s |
| _Sustained TPS_ | 129.08 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 86.18 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 171.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 95.5 |
| _P99_ | 97.3ms |
| _P95_ | 97.1ms |
| _P50_ | 96.3ms |
| _Tx validation time p50 (ms)_ | 30.5 |
| _End-to-end TPS_ | 919.27 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 20.43 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.6 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 27.5 |
| _P99_ | 33.8ms |
| _P95_ | 33.0ms |
| _P50_ | 27.5ms |
| _Tx validation time p50 (ms)_ | 7.5 |
| _End-to-end TPS_ | 107.04 tx/s |
| _Sustained TPS_ | 106.66 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 72.55 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 171.3 |
| _P99_ | 172.8ms |
| _P95_ | 172.5ms |
| _P50_ | 171.8ms |
| _Tx validation time p50 (ms)_ | 84.5 |
| _End-to-end TPS_ | 516.25 tx/s |
| _Backlog drain time (s)_ | 0.2 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 11.47 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.6 |
| _P99_ | 36.8ms |
| _P95_ | 33.0ms |
| _P50_ | 25.1ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 115.56 tx/s |
| _Sustained TPS_ | 113.80 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 78.32 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
