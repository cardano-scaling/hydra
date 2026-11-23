--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-11-23 14:32:24.769639491 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, fire and forget | 30 | 0.0 | 1491.37 | n/a | 19.6 | 19.9 |
| Nodes=1, Constant, wait for tx valid | 30 | 0.1 | 229.29 | 233.78 | 4.3 | 7.5 |
| Nodes=1, Growing, fire and forget | 30 | 0.0 | 1394.39 | n/a | 20.9 | 21.3 |
| Nodes=1, Growing, wait for tx valid | 30 | 0.1 | 218.46 | 217.85 | 4.5 | 5.4 |
| Nodes=1, Mixed, fire and forget | 30 | 0.0 | 1370.04 | n/a | 21.4 | 21.8 |
| Nodes=1, Mixed, wait for tx valid | 30 | 0.1 | 216.66 | 215.00 | 4.6 | 6.5 |
| Nodes=2, Constant, fire and forget | 60 | 0.0 | 1369.98 | n/a | 42.3 | 43.6 |
| Nodes=2, Constant, wait for tx valid | 60 | 0.6 | 96.92 | 89.01 | 20.5 | 118.1 |
| Nodes=2, Growing, fire and forget | 60 | 0.1 | 1191.09 | n/a | 49.2 | 50.2 |
| Nodes=2, Growing, wait for tx valid | 60 | 0.5 | 128.94 | 126.72 | 15.3 | 21.2 |
| Nodes=2, Mixed, fire and forget | 60 | 0.1 | 1099.25 | n/a | 53.4 | 54.4 |
| Nodes=2, Mixed, wait for tx valid | 60 | 0.4 | 137.89 | 134.57 | 14.3 | 18.8 |
| Nodes=3, Constant, fire and forget | 90 | 0.1 | 972.69 | n/a | 90.8 | 92.3 |
| Nodes=3, Constant, wait for tx valid | 90 | 0.7 | 136.92 | 136.51 | 21.6 | 29.2 |
| Nodes=3, Growing, fire and forget | 90 | 0.1 | 1068.48 | n/a | 81.0 | 84.0 |
| Nodes=3, Growing, wait for tx valid | 90 | 0.8 | 114.77 | 113.18 | 25.9 | 32.7 |
| Nodes=3, Mixed, fire and forget | 90 | 0.1 | 1008.17 | n/a | 86.9 | 89.0 |
| Nodes=3, Mixed, wait for tx valid | 90 | 0.8 | 114.55 | 117.20 | 25.8 | 36.1 |


## Nodes=1, Constant, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 19.6 |
| _P99_ | 20.0ms |
| _P95_ | 19.9ms |
| _P50_ | 19.7ms |
| _Tx validation time p50 (ms)_ | 6.0 |
| _End-to-end TPS_ | 1491.37 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 99.42 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 171.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Constant, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.3 |
| _P99_ | 8.3ms |
| _P95_ | 7.5ms |
| _P50_ | 3.9ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 229.29 tx/s |
| _Sustained TPS_ | 233.78 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 229.29 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.8 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Growing, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 20.9 |
| _P99_ | 21.4ms |
| _P95_ | 21.3ms |
| _P50_ | 21.1ms |
| _Tx validation time p50 (ms)_ | 8.5 |
| _End-to-end TPS_ | 1394.39 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 92.96 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 172.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 31 |
      

## Nodes=1, Growing, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.5 |
| _P99_ | 6.0ms |
| _P95_ | 5.4ms |
| _P50_ | 4.4ms |
| _Tx validation time p50 (ms)_ | 1.3 |
| _End-to-end TPS_ | 218.46 tx/s |
| _Sustained TPS_ | 217.85 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 218.46 /s |
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
| _Avg. Confirmation Time (ms)_ | 21.4 |
| _P99_ | 21.8ms |
| _P95_ | 21.8ms |
| _P50_ | 21.6ms |
| _Tx validation time p50 (ms)_ | 8.3 |
| _End-to-end TPS_ | 1370.04 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 91.34 /s |
| _Avg txs per snapshot_ | 15.0 |
| _Peak node RSS (MB)_ | 170.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=1, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 4.6 |
| _P99_ | 6.7ms |
| _P95_ | 6.5ms |
| _P50_ | 4.2ms |
| _Tx validation time p50 (ms)_ | 1.4 |
| _End-to-end TPS_ | 216.66 tx/s |
| _Sustained TPS_ | 215.00 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 30 |
| _Snapshots per second_ | 216.66 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 171.9 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 1 |
      

## Nodes=2, Constant, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 42.3 |
| _P99_ | 43.6ms |
| _P95_ | 43.6ms |
| _P50_ | 42.9ms |
| _Tx validation time p50 (ms)_ | 23.1 |
| _End-to-end TPS_ | 1369.98 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 45.67 /s |
| _Avg txs per snapshot_ | 30.0 |
| _Peak node RSS (MB)_ | 172.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=2, Constant, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 20.5 |
| _P99_ | 152.0ms |
| _P95_ | 118.1ms |
| _P50_ | 10.9ms |
| _Tx validation time p50 (ms)_ | 3.0 |
| _End-to-end TPS_ | 96.92 tx/s |
| _Sustained TPS_ | 89.01 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 96.92 /s |
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
| _Avg. Confirmation Time (ms)_ | 49.2 |
| _P99_ | 50.2ms |
| _P95_ | 50.2ms |
| _P50_ | 49.4ms |
| _Tx validation time p50 (ms)_ | 20.3 |
| _End-to-end TPS_ | 1191.09 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 39.70 /s |
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
| _Avg. Confirmation Time (ms)_ | 15.3 |
| _P99_ | 22.3ms |
| _P95_ | 21.2ms |
| _P50_ | 14.9ms |
| _Tx validation time p50 (ms)_ | 4.1 |
| _End-to-end TPS_ | 128.94 tx/s |
| _Sustained TPS_ | 126.72 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 128.94 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.4 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 62 |
      

## Nodes=2, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 53.4 |
| _P99_ | 54.5ms |
| _P95_ | 54.4ms |
| _P50_ | 53.6ms |
| _Tx validation time p50 (ms)_ | 19.6 |
| _End-to-end TPS_ | 1099.25 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 36.64 /s |
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
| _Avg. Confirmation Time (ms)_ | 14.3 |
| _P99_ | 20.4ms |
| _P95_ | 18.8ms |
| _P50_ | 14.0ms |
| _Tx validation time p50 (ms)_ | 4.7 |
| _End-to-end TPS_ | 137.89 tx/s |
| _Sustained TPS_ | 134.57 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 137.89 /s |
| _Avg txs per snapshot_ | 1.0 |
| _Peak node RSS (MB)_ | 172.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 2 |
      

## Nodes=3, Constant, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 90.8 |
| _P99_ | 92.4ms |
| _P95_ | 92.3ms |
| _P50_ | 91.1ms |
| _Tx validation time p50 (ms)_ | 37.1 |
| _End-to-end TPS_ | 972.69 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 21.62 /s |
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
| _Avg. Confirmation Time (ms)_ | 21.6 |
| _P99_ | 31.5ms |
| _P95_ | 29.2ms |
| _P50_ | 21.0ms |
| _Tx validation time p50 (ms)_ | 5.6 |
| _End-to-end TPS_ | 136.92 tx/s |
| _Sustained TPS_ | 136.51 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 92.80 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Growing, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 81.0 |
| _P99_ | 84.1ms |
| _P95_ | 84.0ms |
| _P50_ | 80.9ms |
| _Tx validation time p50 (ms)_ | 29.4 |
| _End-to-end TPS_ | 1068.48 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 23.74 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 172.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.9 |
| _P99_ | 34.7ms |
| _P95_ | 32.7ms |
| _P50_ | 25.8ms |
| _Tx validation time p50 (ms)_ | 7.3 |
| _End-to-end TPS_ | 114.77 tx/s |
| _Sustained TPS_ | 113.18 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 60 |
| _Snapshots per second_ | 76.51 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 174.0 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | open-loop |
| _Avg. Confirmation Time (ms)_ | 86.9 |
| _P99_ | 89.2ms |
| _P95_ | 89.0ms |
| _P50_ | 87.5ms |
| _Tx validation time p50 (ms)_ | 27.2 |
| _End-to-end TPS_ | 1008.17 tx/s |
| _Backlog drain time (s)_ | 0.1 |
| _Snapshots observed_ | 2 |
| _Snapshots per second_ | 22.40 /s |
| _Avg txs per snapshot_ | 45.0 |
| _Peak node RSS (MB)_ | 171.3 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      

## Nodes=3, Mixed, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Load mode_ | closed-loop |
| _Avg. Confirmation Time (ms)_ | 25.8 |
| _P99_ | 45.6ms |
| _P95_ | 36.1ms |
| _P50_ | 24.5ms |
| _Tx validation time p50 (ms)_ | 6.6 |
| _End-to-end TPS_ | 114.55 tx/s |
| _Sustained TPS_ | 117.20 tx/s |
| _Backlog drain time (s)_ | 0.0 |
| _Snapshots observed_ | 61 |
| _Snapshots per second_ | 77.64 /s |
| _Avg txs per snapshot_ | 1.5 |
| _Peak node RSS (MB)_ | 173.5 |
| _Number of Invalid txs_ | 0 |
| _Refused submissions_ | 0 |
| _Fanout outputs_        | 3 |
      
