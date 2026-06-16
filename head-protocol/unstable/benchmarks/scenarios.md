--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-16 10:30:19.019188305 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 481.66 | 1540.62 | 61.0 | 62.0 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 181.34 | 186.01 | 5.4 | 6.6 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 449.93 | 917.09 | 65.0 | 66.4 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 125.73 | 124.71 | 7.8 | 9.8 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 468.31 | 1712.39 | 63.0 | 63.8 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 142.02 | 147.16 | 6.9 | 8.8 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 344.66 | 1632.05 | 171.3 | 172.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 112.82 | 117.76 | 17.5 | 22.9 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 308.70 | 579.35 | 192.0 | 193.3 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 63.04 | 62.89 | 31.1 | 44.2 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 349.91 | 1083.61 | 169.1 | 171.3 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.7 | 83.97 | 84.16 | 23.5 | 30.4 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 331.82 | 1257.92 | 267.3 | 269.1 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 93.27 | 79.30 | 31.8 | 40.1 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 266.85 | 410.33 | 331.3 | 333.1 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.0 | 45.37 | 46.51 | 63.5 | 94.8 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 304.37 | 832.39 | 291.9 | 295.5 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 61.88 | 59.37 | 47.6 | 67.0 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 61.0 |
| _P99_ | 62.1ms |
| _P95_ | 62.0ms |
| _P50_ | 61.3ms |
| _End-to-end TPS_ | 481.66 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1540.62 tx/s |
| _Per-snapshot TPS P95_ | 2910.12 tx/s |
| _Per-snapshot TPS max_ | 3031.86 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.4 |
| _P99_ | 7.9ms |
| _P95_ | 6.6ms |
| _P50_ | 5.3ms |
| _End-to-end TPS_ | 181.34 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 186.01 tx/s |
| _Per-snapshot TPS P95_ | 209.91 tx/s |
| _Per-snapshot TPS max_ | 210.88 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.0 |
| _P99_ | 66.5ms |
| _P95_ | 66.4ms |
| _P50_ | 65.1ms |
| _End-to-end TPS_ | 449.93 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 917.09 tx/s |
| _Per-snapshot TPS P95_ | 1724.72 tx/s |
| _Per-snapshot TPS max_ | 1796.50 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.8 |
| _P99_ | 10.2ms |
| _P95_ | 9.8ms |
| _P50_ | 7.9ms |
| _End-to-end TPS_ | 125.73 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 124.71 tx/s |
| _Per-snapshot TPS P95_ | 169.05 tx/s |
| _Per-snapshot TPS max_ | 169.52 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 63.0 |
| _P99_ | 63.8ms |
| _P95_ | 63.8ms |
| _P50_ | 63.2ms |
| _End-to-end TPS_ | 468.31 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1712.39 tx/s |
| _Per-snapshot TPS P95_ | 3237.33 tx/s |
| _Per-snapshot TPS max_ | 3372.88 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 6.9 |
| _P99_ | 11.5ms |
| _P95_ | 8.8ms |
| _P50_ | 6.7ms |
| _End-to-end TPS_ | 142.02 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 147.16 tx/s |
| _Per-snapshot TPS P95_ | 181.32 tx/s |
| _Per-snapshot TPS max_ | 196.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 171.3 |
| _P99_ | 172.9ms |
| _P95_ | 172.9ms |
| _P50_ | 170.9ms |
| _End-to-end TPS_ | 344.66 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1632.05 tx/s |
| _Per-snapshot TPS P95_ | 3095.00 tx/s |
| _Per-snapshot TPS max_ | 3225.04 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 17.5 |
| _P99_ | 24.3ms |
| _P95_ | 22.9ms |
| _P50_ | 17.2ms |
| _End-to-end TPS_ | 112.82 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 117.76 tx/s |
| _Per-snapshot TPS P95_ | 150.72 tx/s |
| _Per-snapshot TPS max_ | 156.37 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 192.0 |
| _P99_ | 193.4ms |
| _P95_ | 193.3ms |
| _P50_ | 192.8ms |
| _End-to-end TPS_ | 308.70 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 579.35 tx/s |
| _Per-snapshot TPS P95_ | 1094.44 tx/s |
| _Per-snapshot TPS max_ | 1140.23 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 31.1 |
| _P99_ | 45.9ms |
| _P95_ | 44.2ms |
| _P50_ | 30.7ms |
| _End-to-end TPS_ | 63.04 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 62.89 tx/s |
| _Per-snapshot TPS P95_ | 100.65 tx/s |
| _Per-snapshot TPS max_ | 114.19 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 169.1 |
| _P99_ | 171.4ms |
| _P95_ | 171.3ms |
| _P50_ | 169.6ms |
| _End-to-end TPS_ | 349.91 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1083.61 tx/s |
| _Per-snapshot TPS P95_ | 2052.52 tx/s |
| _Per-snapshot TPS max_ | 2138.65 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 23.5 |
| _P99_ | 33.8ms |
| _P95_ | 30.4ms |
| _P50_ | 23.7ms |
| _End-to-end TPS_ | 83.97 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 84.16 tx/s |
| _Per-snapshot TPS P95_ | 125.18 tx/s |
| _Per-snapshot TPS max_ | 134.40 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 267.3 |
| _P99_ | 269.2ms |
| _P95_ | 269.1ms |
| _P50_ | 267.7ms |
| _End-to-end TPS_ | 331.82 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1257.92 tx/s |
| _Per-snapshot TPS P95_ | 2386.16 tx/s |
| _Per-snapshot TPS max_ | 2486.44 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 31.8 |
| _P99_ | 46.3ms |
| _P95_ | 40.1ms |
| _P50_ | 31.6ms |
| _End-to-end TPS_ | 93.27 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 79.30 tx/s |
| _Per-snapshot TPS P95_ | 173.95 tx/s |
| _Per-snapshot TPS max_ | 184.00 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 331.3 |
| _P99_ | 333.9ms |
| _P95_ | 333.1ms |
| _P50_ | 332.4ms |
| _End-to-end TPS_ | 266.85 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 410.33 tx/s |
| _Per-snapshot TPS P95_ | 775.61 tx/s |
| _Per-snapshot TPS max_ | 808.08 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 63.5 |
| _P99_ | 131.1ms |
| _P95_ | 94.8ms |
| _P50_ | 59.3ms |
| _End-to-end TPS_ | 45.37 tx/s |
| _Snapshots observed_ | 63 |
| _Per-snapshot TPS P50_ | 46.51 tx/s |
| _Per-snapshot TPS P95_ | 90.45 tx/s |
| _Per-snapshot TPS max_ | 102.85 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 291.9 |
| _P99_ | 295.6ms |
| _P95_ | 295.5ms |
| _P50_ | 291.1ms |
| _End-to-end TPS_ | 304.37 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 832.39 tx/s |
| _Per-snapshot TPS P95_ | 1577.76 tx/s |
| _Per-snapshot TPS max_ | 1644.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 47.6 |
| _P99_ | 81.8ms |
| _P95_ | 67.0ms |
| _P50_ | 46.4ms |
| _End-to-end TPS_ | 61.88 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 59.37 tx/s |
| _Per-snapshot TPS P95_ | 129.03 tx/s |
| _Per-snapshot TPS max_ | 156.41 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
