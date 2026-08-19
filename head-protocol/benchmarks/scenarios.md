--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-07-15 12:47:39.333921371 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Peak sustained TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 450.68 | 30.00 | 65.5 | 66.2 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 138.13 | 30.00 | 7.1 | 12.8 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 350.06 | 30.00 | 84.1 | 85.4 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 103.77 | 30.00 | 9.5 | 12.3 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 389.82 | 30.00 | 75.7 | 76.6 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 130.69 | 30.00 | 7.6 | 10.9 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 338.18 | 60.00 | 175.8 | 176.3 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 102.97 | 60.00 | 19.2 | 28.0 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 291.44 | 60.00 | 203.9 | 204.9 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 59.12 | 59.00 | 33.2 | 48.8 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 348.54 | 60.00 | 170.0 | 170.8 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.8 | 75.32 | 60.00 | 26.3 | 35.2 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 320.49 | 90.00 | 278.0 | 279.9 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 89.11 | 90.00 | 33.1 | 44.1 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 241.80 | 90.00 | 366.3 | 368.3 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.18 | 55.00 | 66.9 | 100.2 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 286.48 | 90.00 | 311.1 | 313.4 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 61.06 | 59.00 | 48.4 | 64.3 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.5 |
| _P99_ | 66.3ms |
| _P95_ | 66.2ms |
| _P50_ | 65.7ms |
| _End-to-end TPS_ | 450.68 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.1 |
| _P99_ | 18.0ms |
| _P95_ | 12.8ms |
| _P50_ | 6.2ms |
| _End-to-end TPS_ | 138.13 tx/s |
| _Snapshots observed_ | 30 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 84.1 |
| _P99_ | 85.5ms |
| _P95_ | 85.4ms |
| _P50_ | 85.0ms |
| _End-to-end TPS_ | 350.06 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 9.5 |
| _P99_ | 16.8ms |
| _P95_ | 12.3ms |
| _P50_ | 9.6ms |
| _End-to-end TPS_ | 103.77 tx/s |
| _Snapshots observed_ | 30 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 75.7 |
| _P99_ | 76.7ms |
| _P95_ | 76.6ms |
| _P50_ | 76.3ms |
| _End-to-end TPS_ | 389.82 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.6 |
| _P99_ | 16.2ms |
| _P95_ | 10.9ms |
| _P50_ | 7.3ms |
| _End-to-end TPS_ | 130.69 tx/s |
| _Snapshots observed_ | 30 |
| _Peak sustained TPS_ | 30.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 175.8 |
| _P99_ | 176.7ms |
| _P95_ | 176.3ms |
| _P50_ | 176.0ms |
| _End-to-end TPS_ | 338.18 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 60.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 19.2 |
| _P99_ | 30.6ms |
| _P95_ | 28.0ms |
| _P50_ | 18.0ms |
| _End-to-end TPS_ | 102.97 tx/s |
| _Snapshots observed_ | 60 |
| _Peak sustained TPS_ | 60.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 203.9 |
| _P99_ | 205.0ms |
| _P95_ | 204.9ms |
| _P50_ | 204.6ms |
| _End-to-end TPS_ | 291.44 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 60.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 33.2 |
| _P99_ | 53.0ms |
| _P95_ | 48.8ms |
| _P50_ | 33.6ms |
| _End-to-end TPS_ | 59.12 tx/s |
| _Snapshots observed_ | 60 |
| _Peak sustained TPS_ | 59.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 170.0 |
| _P99_ | 171.4ms |
| _P95_ | 170.8ms |
| _P50_ | 170.2ms |
| _End-to-end TPS_ | 348.54 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 60.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 26.3 |
| _P99_ | 44.7ms |
| _P95_ | 35.2ms |
| _P50_ | 26.1ms |
| _End-to-end TPS_ | 75.32 tx/s |
| _Snapshots observed_ | 60 |
| _Peak sustained TPS_ | 60.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 278.0 |
| _P99_ | 280.0ms |
| _P95_ | 279.9ms |
| _P50_ | 279.2ms |
| _End-to-end TPS_ | 320.49 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 90.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.1 |
| _P99_ | 48.7ms |
| _P95_ | 44.1ms |
| _P50_ | 31.9ms |
| _End-to-end TPS_ | 89.11 tx/s |
| _Snapshots observed_ | 61 |
| _Peak sustained TPS_ | 90.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 366.3 |
| _P99_ | 368.8ms |
| _P95_ | 368.3ms |
| _P50_ | 367.0ms |
| _End-to-end TPS_ | 241.80 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 90.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 66.9 |
| _P99_ | 127.0ms |
| _P95_ | 100.2ms |
| _P50_ | 62.8ms |
| _End-to-end TPS_ | 43.18 tx/s |
| _Snapshots observed_ | 64 |
| _Peak sustained TPS_ | 55.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 93 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 311.1 |
| _P99_ | 313.4ms |
| _P95_ | 313.4ms |
| _P50_ | 310.9ms |
| _End-to-end TPS_ | 286.48 tx/s |
| _Snapshots observed_ | 2 |
| _Peak sustained TPS_ | 90.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 48.4 |
| _P99_ | 72.1ms |
| _P95_ | 64.3ms |
| _P50_ | 49.6ms |
| _End-to-end TPS_ | 61.06 tx/s |
| _Snapshots observed_ | 61 |
| _Peak sustained TPS_ | 59.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
