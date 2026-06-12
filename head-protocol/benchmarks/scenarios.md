--- 
sidebar_label: 'Scenario benchmarks' 
sidebar_position: 5 
--- 

# Scenario benchmark results 

This page collects results from the scenario matrix: every combination  of cluster size, UTxO shape, and incremental-ops mode is exercised by  CI from the latest `master` code and reported below.

:::caution

Numbers are approximate. They come from cloud VMs rather than  controlled hardware, so the useful signal is the relative change  between cells and between commits, not the absolute throughput.

:::

_Generated at_  2026-06-12 15:46:32.107895274 UTC


## Summary across cells

TPS columns are rates (transactions per second); _Wall clock (s)_ is the measured elapsed time from the first tx submission to the last confirmation. Times are rounded to one decimal.

| Scenario | Txs | Wall clock (s) | End-to-end TPS (tx/s) | Per-snapshot p50 TPS (tx/s) | Avg conf (ms) | P95 conf (ms) |
| -- | -- | -- | -- | -- | -- | -- |
| Nodes=1, Constant, incremental ops off, fire and forget | 30 | 0.1 | 489.70 | 1641.04 | 60.3 | 61.0 |
| Nodes=1, Constant, incremental ops off, wait for tx valid | 30 | 0.2 | 171.51 | 178.96 | 5.8 | 7.1 |
| Nodes=1, Growing, incremental ops off, fire and forget | 30 | 0.1 | 440.91 | 928.49 | 66.8 | 67.8 |
| Nodes=1, Growing, incremental ops off, wait for tx valid | 30 | 0.3 | 117.69 | 119.53 | 8.4 | 11.3 |
| Nodes=1, Mixed, incremental ops off, fire and forget | 30 | 0.1 | 407.95 | 1543.88 | 72.8 | 73.4 |
| Nodes=1, Mixed, incremental ops off, wait for tx valid | 30 | 0.2 | 141.69 | 144.35 | 7.0 | 8.4 |
| Nodes=2, Constant, incremental ops off, fire and forget | 60 | 0.2 | 354.08 | 1254.95 | 167.4 | 168.9 |
| Nodes=2, Constant, incremental ops off, wait for tx valid | 60 | 0.6 | 105.71 | 111.01 | 18.7 | 24.5 |
| Nodes=2, Growing, incremental ops off, fire and forget | 60 | 0.2 | 319.94 | 734.48 | 185.3 | 186.7 |
| Nodes=2, Growing, incremental ops off, wait for tx valid | 60 | 1.0 | 61.34 | 62.91 | 32.1 | 46.2 |
| Nodes=2, Mixed, incremental ops off, fire and forget | 60 | 0.2 | 343.33 | 1236.89 | 172.6 | 174.4 |
| Nodes=2, Mixed, incremental ops off, wait for tx valid | 60 | 0.7 | 83.48 | 87.86 | 23.7 | 31.7 |
| Nodes=3, Constant, incremental ops off, fire and forget | 90 | 0.3 | 300.04 | 894.21 | 295.4 | 299.6 |
| Nodes=3, Constant, incremental ops off, wait for tx valid | 90 | 1.0 | 87.98 | 82.33 | 33.9 | 43.2 |
| Nodes=3, Growing, incremental ops off, fire and forget | 90 | 0.4 | 222.12 | 397.30 | 400.6 | 404.1 |
| Nodes=3, Growing, incremental ops off, wait for tx valid | 90 | 2.1 | 43.57 | 43.13 | 66.8 | 100.9 |
| Nodes=3, Mixed, incremental ops off, fire and forget | 90 | 0.3 | 293.15 | 807.19 | 302.6 | 306.0 |
| Nodes=3, Mixed, incremental ops off, wait for tx valid | 90 | 1.5 | 59.85 | 54.75 | 49.6 | 73.0 |


## Nodes=1, Constant, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 60.3 |
| _P99_ | 61.0ms |
| _P95_ | 61.0ms |
| _P50_ | 60.5ms |
| _End-to-end TPS_ | 489.70 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1641.04 tx/s |
| _Per-snapshot TPS P95_ | 3100.79 tx/s |
| _Per-snapshot TPS max_ | 3230.55 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Constant, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 5.8 |
| _P99_ | 7.9ms |
| _P95_ | 7.1ms |
| _P50_ | 5.5ms |
| _End-to-end TPS_ | 171.51 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 178.96 tx/s |
| _Per-snapshot TPS P95_ | 195.43 tx/s |
| _Per-snapshot TPS max_ | 197.04 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, fire and forget



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 66.8 |
| _P99_ | 67.8ms |
| _P95_ | 67.8ms |
| _P50_ | 67.3ms |
| _End-to-end TPS_ | 440.91 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 928.49 tx/s |
| _Per-snapshot TPS P95_ | 1746.91 tx/s |
| _Per-snapshot TPS max_ | 1819.66 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Growing, incremental ops off, wait for tx valid



| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 8.4 |
| _P99_ | 12.5ms |
| _P95_ | 11.3ms |
| _P50_ | 8.3ms |
| _End-to-end TPS_ | 117.69 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 119.53 tx/s |
| _Per-snapshot TPS P95_ | 152.96 tx/s |
| _Per-snapshot TPS max_ | 161.25 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 72.8 |
| _P99_ | 73.4ms |
| _P95_ | 73.4ms |
| _P50_ | 73.1ms |
| _End-to-end TPS_ | 407.95 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1543.88 tx/s |
| _Per-snapshot TPS P95_ | 2919.33 tx/s |
| _Per-snapshot TPS max_ | 3041.60 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=1, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  1 | 
| -- | -- |
| _Number of txs_ | 30 |
| _Avg. Confirmation Time (ms)_ | 7.0 |
| _P99_ | 9.3ms |
| _P95_ | 8.4ms |
| _P50_ | 6.9ms |
| _End-to-end TPS_ | 141.69 tx/s |
| _Snapshots observed_ | 30 |
| _Per-snapshot TPS P50_ | 144.35 tx/s |
| _Per-snapshot TPS P95_ | 174.42 tx/s |
| _Per-snapshot TPS max_ | 182.18 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 167.4 |
| _P99_ | 168.9ms |
| _P95_ | 168.9ms |
| _P50_ | 167.9ms |
| _End-to-end TPS_ | 354.08 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1254.95 tx/s |
| _Per-snapshot TPS P95_ | 2378.20 tx/s |
| _Per-snapshot TPS max_ | 2478.04 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Constant, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 18.7 |
| _P99_ | 31.9ms |
| _P95_ | 24.5ms |
| _P50_ | 18.0ms |
| _End-to-end TPS_ | 105.71 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 111.01 tx/s |
| _Per-snapshot TPS P95_ | 146.95 tx/s |
| _Per-snapshot TPS max_ | 158.44 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, fire and forget



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 185.3 |
| _P99_ | 186.8ms |
| _P95_ | 186.7ms |
| _P50_ | 185.7ms |
| _End-to-end TPS_ | 319.94 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 734.48 tx/s |
| _Per-snapshot TPS P95_ | 1389.33 tx/s |
| _Per-snapshot TPS max_ | 1447.54 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Growing, incremental ops off, wait for tx valid



| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 32.1 |
| _P99_ | 50.4ms |
| _P95_ | 46.2ms |
| _P50_ | 32.2ms |
| _End-to-end TPS_ | 61.34 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 62.91 tx/s |
| _Per-snapshot TPS P95_ | 101.70 tx/s |
| _Per-snapshot TPS max_ | 124.88 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 172.6 |
| _P99_ | 174.5ms |
| _P95_ | 174.4ms |
| _P50_ | 173.0ms |
| _End-to-end TPS_ | 343.33 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 1236.89 tx/s |
| _Per-snapshot TPS P95_ | 2344.09 tx/s |
| _Per-snapshot TPS max_ | 2442.51 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=2, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  2 | 
| -- | -- |
| _Number of txs_ | 60 |
| _Avg. Confirmation Time (ms)_ | 23.7 |
| _P99_ | 35.9ms |
| _P95_ | 31.7ms |
| _P50_ | 22.3ms |
| _End-to-end TPS_ | 83.48 tx/s |
| _Snapshots observed_ | 60 |
| _Per-snapshot TPS P50_ | 87.86 tx/s |
| _Per-snapshot TPS P95_ | 116.49 tx/s |
| _Per-snapshot TPS max_ | 156.55 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 295.4 |
| _P99_ | 299.6ms |
| _P95_ | 299.6ms |
| _P50_ | 297.0ms |
| _End-to-end TPS_ | 300.04 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 894.21 tx/s |
| _Per-snapshot TPS P95_ | 1695.33 tx/s |
| _Per-snapshot TPS max_ | 1766.54 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Constant, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 33.9 |
| _P99_ | 51.4ms |
| _P95_ | 43.2ms |
| _P50_ | 33.9ms |
| _End-to-end TPS_ | 87.98 tx/s |
| _Snapshots observed_ | 61 |
| _Per-snapshot TPS P50_ | 82.33 tx/s |
| _Per-snapshot TPS P95_ | 166.31 tx/s |
| _Per-snapshot TPS max_ | 181.70 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, fire and forget



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 400.6 |
| _P99_ | 404.3ms |
| _P95_ | 404.1ms |
| _P50_ | 401.4ms |
| _End-to-end TPS_ | 222.12 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 397.30 tx/s |
| _Per-snapshot TPS P95_ | 751.76 tx/s |
| _Per-snapshot TPS max_ | 783.27 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Growing, incremental ops off, wait for tx valid



| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 66.8 |
| _P99_ | 117.6ms |
| _P95_ | 100.9ms |
| _P50_ | 66.6ms |
| _End-to-end TPS_ | 43.57 tx/s |
| _Snapshots observed_ | 64 |
| _Per-snapshot TPS P50_ | 43.13 tx/s |
| _Per-snapshot TPS P95_ | 96.57 tx/s |
| _Per-snapshot TPS max_ | 123.84 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, fire and forget

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 302.6 |
| _P99_ | 306.1ms |
| _P95_ | 306.0ms |
| _P50_ | 305.0ms |
| _End-to-end TPS_ | 293.15 tx/s |
| _Snapshots observed_ | 2 |
| _Per-snapshot TPS P50_ | 807.19 tx/s |
| _Per-snapshot TPS P95_ | 1530.01 tx/s |
| _Per-snapshot TPS max_ | 1594.26 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      

## Nodes=3, Mixed, incremental ops off, wait for tx valid

Each client first grows its UTxO set (1-in to 2-out) for half of its tx budget, then contracts it back (2-in to 1-out) for the remainder.

| Number of nodes |  3 | 
| -- | -- |
| _Number of txs_ | 90 |
| _Avg. Confirmation Time (ms)_ | 49.6 |
| _P99_ | 79.3ms |
| _P95_ | 73.0ms |
| _P50_ | 47.5ms |
| _End-to-end TPS_ | 59.85 tx/s |
| _Snapshots observed_ | 62 |
| _Per-snapshot TPS P50_ | 54.75 tx/s |
| _Per-snapshot TPS P95_ | 120.67 tx/s |
| _Per-snapshot TPS max_ | 162.02 tx/s |
| _Number of Invalid txs_ | 0 |
| _Fanout outputs_        | 0 |
      
