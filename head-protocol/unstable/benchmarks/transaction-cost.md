--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 22:00:06.294202515 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340 | 14599 | 
| μHead | fd173b993e12103cd734ca6710d364e17120a5eb37a224c64ab2b188* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5838 | 10.78 | 3.43 | 0.52 |
| 2| 6038 | 12.84 | 4.08 | 0.55 |
| 3| 6238 | 15.05 | 4.78 | 0.58 |
| 5| 6638 | 18.88 | 5.97 | 0.64 |
| 10| 7644 | 29.00 | 9.14 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 169 | 747 | 43.94 | 12.60 | 0.63 |
| 4 | 224 | 858 | 49.57 | 14.34 | 0.69 |
| 5 | 281 | 974 | 57.90 | 16.67 | 0.78 |
| 6 | 338 | 1081 | 66.22 | 19.09 | 0.87 |
| 7 | 393 | 1192 | 73.06 | 21.30 | 0.95 |
| 8 | 450 | 1303 | 85.24 | 24.48 | 1.07 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.92 | 7.32 | 0.47 |
| 2| 1964 | 26.47 | 8.98 | 0.52 |
| 3| 2151 | 28.01 | 10.07 | 0.54 |
| 5| 2383 | 31.12 | 12.27 | 0.59 |
| 10| 3126 | 41.22 | 18.41 | 0.75 |
| 41| 7485 | 94.67 | 53.96 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.81 | 7.37 | 0.42 |
| 2| 768 | 24.31 | 8.45 | 0.44 |
| 3| 916 | 26.71 | 9.80 | 0.48 |
| 5| 1206 | 29.94 | 12.02 | 0.53 |
| 10| 1931 | 38.51 | 17.76 | 0.67 |
| 43| 6761 | 98.13 | 56.34 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 29.17 | 8.91 | 0.48 |
| 2| 771 | 28.51 | 9.39 | 0.48 |
| 3| 1072 | 32.37 | 11.19 | 0.54 |
| 5| 1313 | 37.58 | 13.96 | 0.61 |
| 10| 2020 | 45.01 | 19.39 | 0.74 |
| 36| 6014 | 97.96 | 51.62 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 33.87 | 10.16 | 0.53 |
| 2| 878 | 36.64 | 11.62 | 0.57 |
| 3| 966 | 37.95 | 12.63 | 0.59 |
| 5| 1267 | 42.60 | 15.27 | 0.66 |
| 10| 2108 | 55.51 | 22.25 | 0.85 |
| 28| 4893 | 97.83 | 46.06 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 27.13 | 9.10 | 0.69 |
| 2| 5953 | 35.83 | 12.04 | 0.79 |
| 3| 6234 | 47.28 | 15.97 | 0.92 |
| 4| 6209 | 53.89 | 18.12 | 0.99 |
| 5| 6384 | 62.70 | 21.05 | 1.09 |
| 6| 6554 | 70.39 | 23.67 | 1.18 |
| 7| 6881 | 86.85 | 29.39 | 1.37 |
| 8| 6850 | 89.44 | 30.06 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 30 | 1710 | 6857 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2219 | 7159 | 99.56 | 38.10 | 1.54 |

