--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-12 16:40:35.696495017 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 549.01 | 1930.18 | 53.9 | 54.3 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 177.90 | 199.28 | 5.6 | 7.2 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 451.12 | 935.11 | 65.4 | 66.2 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.2 | 121.37 | 122.80 | 8.1 | 10.4 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 500.12 | 1527.63 | 59.1 | 59.7 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 139.02 | 145.58 | 7.1 | 9.8 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.1 | 447.20 | 1456.15 | 132.6 | 133.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.5 | 122.90 | 132.86 | 16.1 | 23.2 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 346.18 | 753.58 | 171.4 | 173.1 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 0.9 | 66.07 | 69.62 | 29.2 | 44.8 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 365.65 | 1060.65 | 162.1 | 163.8 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.7 | 88.11 | 91.16 | 22.4 | 30.5 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 356.82 | 1018.78 | 249.1 | 250.9 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 0.9 | 103.23 | 94.12 | 28.5 | 36.0 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.3 | 264.01 | 457.45 | 335.5 | 337.9 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 1.9 | 46.31 | 44.90 | 60.9 | 97.3 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 302.97 | 982.97 | 294.1 | 294.9 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.3 | 67.00 | 64.54 | 44.3 | 66.9 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 53.9 |
| _P99_ | 54.3ms |
| _P95_ | 54.3ms |
| _P50_ | 54.1ms |
| _End-to-end TPS_ | 549.01 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1930.18 tx/s |
| _Per-snapshot TPS P95_ | 3648.22 tx/s |
| _Per-snapshot TPS max_ | 3800.94 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.6 |
| _P99_ | 9.2ms |
| _P95_ | 7.2ms |
| _P50_ | 5.0ms |
| _End-to-end TPS_ | 177.90 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 199.28 tx/s |
| _Per-snapshot TPS P95_ | 217.43 tx/s |
| _Per-snapshot TPS max_ | 218.75 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 65.4 |
| _P99_ | 66.2ms |
| _P95_ | 66.2ms |
| _P50_ | 65.9ms |
| _End-to-end TPS_ | 451.12 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 935.11 tx/s |
| _Per-snapshot TPS P95_ | 1758.99 tx/s |
| _Per-snapshot TPS max_ | 1832.22 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.1 |
| _P99_ | 11.7ms |
| _P95_ | 10.4ms |
| _P50_ | 8.0ms |
| _End-to-end TPS_ | 121.37 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 122.80 tx/s |
| _Per-snapshot TPS P95_ | 168.08 tx/s |
| _Per-snapshot TPS max_ | 177.87 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 59.1 |
| _P99_ | 59.7ms |
| _P95_ | 59.7ms |
| _P50_ | 59.4ms |
| _End-to-end TPS_ | 500.12 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1527.63 tx/s |
| _Per-snapshot TPS P95_ | 2884.64 tx/s |
| _Per-snapshot TPS max_ | 3005.26 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.1 |
| _P99_ | 11.6ms |
| _P95_ | 9.8ms |
| _P50_ | 6.7ms |
| _End-to-end TPS_ | 139.02 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 145.58 tx/s |
| _Per-snapshot TPS P95_ | 185.61 tx/s |
| _Per-snapshot TPS max_ | 197.54 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 132.6 |
| _P99_ | 134.0ms |
| _P95_ | 133.9ms |
| _P50_ | 132.6ms |
| _End-to-end TPS_ | 447.20 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1456.15 tx/s |
| _Per-snapshot TPS P95_ | 2758.72 tx/s |
| _Per-snapshot TPS max_ | 2874.51 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 16.1 |
| _P99_ | 27.2ms |
| _P95_ | 23.2ms |
| _P50_ | 15.0ms |
| _End-to-end TPS_ | 122.90 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 132.86 tx/s |
| _Per-snapshot TPS P95_ | 167.90 tx/s |
| _Per-snapshot TPS max_ | 174.88 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 171.4 |
| _P99_ | 173.2ms |
| _P95_ | 173.1ms |
| _P50_ | 171.5ms |
| _End-to-end TPS_ | 346.18 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 753.58 tx/s |
| _Per-snapshot TPS P95_ | 1425.04 tx/s |
| _Per-snapshot TPS max_ | 1484.73 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 29.2 |
| _P99_ | 47.5ms |
| _P95_ | 44.8ms |
| _P50_ | 28.9ms |
| _End-to-end TPS_ | 66.07 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 69.62 tx/s |
| _Per-snapshot TPS P95_ | 133.20 tx/s |
| _Per-snapshot TPS max_ | 148.10 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 162.1 |
| _P99_ | 163.9ms |
| _P95_ | 163.8ms |
| _P50_ | 162.6ms |
| _End-to-end TPS_ | 365.65 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1060.65 tx/s |
| _Per-snapshot TPS P95_ | 2008.60 tx/s |
| _Per-snapshot TPS max_ | 2092.86 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 22.4 |
| _P99_ | 32.6ms |
| _P95_ | 30.5ms |
| _P50_ | 22.7ms |
| _End-to-end TPS_ | 88.11 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 91.16 tx/s |
| _Per-snapshot TPS P95_ | 140.99 tx/s |
| _Per-snapshot TPS max_ | 156.05 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 249.1 |
| _P99_ | 251.1ms |
| _P95_ | 250.9ms |
| _P50_ | 250.2ms |
| _End-to-end TPS_ | 356.82 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1018.78 tx/s |
| _Per-snapshot TPS P95_ | 1931.33 tx/s |
| _Per-snapshot TPS max_ | 2012.45 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 28.5 |
| _P99_ | 38.9ms |
| _P95_ | 36.0ms |
| _P50_ | 27.8ms |
| _End-to-end TPS_ | 103.23 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 94.12 tx/s |
| _Per-snapshot TPS P95_ | 189.19 tx/s |
| _Per-snapshot TPS max_ | 214.30 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 335.5 |
| _P99_ | 338.0ms |
| _P95_ | 337.9ms |
| _P50_ | 336.7ms |
| _End-to-end TPS_ | 264.01 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 457.45 tx/s |
| _Per-snapshot TPS P95_ | 865.38 tx/s |
| _Per-snapshot TPS max_ | 901.64 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 60.9 |
| _P99_ | 128.7ms |
| _P95_ | 97.3ms |
| _P50_ | 61.3ms |
| _End-to-end TPS_ | 46.31 tx/s |
| _Snapshots observed_ | 64 |
| _Per-snapshot TPS P50_ | 44.90 tx/s |
| _Per-snapshot TPS P95_ | 112.45 tx/s |
| _Per-snapshot TPS max_ | 150.76 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 294.1 |
| _P99_ | 295.3ms |
| _P95_ | 294.9ms |
| _P50_ | 294.6ms |
| _End-to-end TPS_ | 302.97 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 982.97 tx/s |
| _Per-snapshot TPS P95_ | 1864.04 tx/s |
| _Per-snapshot TPS max_ | 1942.35 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 44.3 |
| _P99_ | 79.5ms |
| _P95_ | 66.9ms |
| _P50_ | 43.1ms |
| _End-to-end TPS_ | 67.00 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 64.54 tx/s |
| _Per-snapshot TPS P95_ | 141.56 tx/s |
| _Per-snapshot TPS max_ | 171.66 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
