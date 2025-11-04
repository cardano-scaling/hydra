--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-04 04:48:35.164197022 UTC |
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
| 1| 5834 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.72 | 4.03 | 0.55 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6646 | 18.71 | 5.91 | 0.64 |
| 10| 7648 | 29.31 | 9.25 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10045 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 42.25 | 12.15 | 0.61 |
| 4 | 227 | 858 | 53.76 | 15.29 | 0.73 |
| 5 | 283 | 969 | 62.33 | 17.73 | 0.82 |
| 6 | 338 | 1081 | 75.79 | 21.46 | 0.96 |
| 7 | 394 | 1196 | 72.61 | 21.01 | 0.94 |
| 8 | 451 | 1303 | 80.66 | 23.38 | 1.03 |
| 9 | 505 | 1418 | 90.88 | 26.28 | 1.14 |
| 10 | 560 | 1525 | 96.65 | 27.95 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.29 | 7.69 | 0.48 |
| 2| 1883 | 24.85 | 8.50 | 0.50 |
| 3| 2098 | 27.98 | 10.06 | 0.54 |
| 5| 2391 | 30.96 | 12.23 | 0.59 |
| 10| 3157 | 40.90 | 18.33 | 0.75 |
| 39| 7542 | 96.88 | 53.27 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.80 | 7.38 | 0.41 |
| 2| 758 | 23.66 | 8.26 | 0.43 |
| 3| 902 | 25.72 | 9.52 | 0.47 |
| 5| 1325 | 32.37 | 12.70 | 0.56 |
| 10| 2024 | 39.67 | 18.08 | 0.69 |
| 42| 6741 | 99.28 | 55.97 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 668 | 29.13 | 8.90 | 0.48 |
| 2| 885 | 29.93 | 9.83 | 0.50 |
| 3| 902 | 30.23 | 10.54 | 0.51 |
| 5| 1331 | 38.38 | 14.19 | 0.62 |
| 10| 2136 | 46.59 | 19.88 | 0.76 |
| 36| 5950 | 97.31 | 51.44 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.79 | 10.15 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 896 | 37.24 | 12.41 | 0.58 |
| 5| 1230 | 41.82 | 15.03 | 0.65 |
| 10| 2048 | 54.96 | 22.07 | 0.84 |
| 28| 4720 | 96.44 | 45.62 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 6024 | 37.00 | 12.46 | 0.80 |
| 3| 6138 | 45.70 | 15.39 | 0.90 |
| 4| 6263 | 54.91 | 18.49 | 1.00 |
| 5| 6360 | 63.97 | 21.51 | 1.10 |
| 6| 6538 | 70.30 | 23.60 | 1.18 |
| 7| 6903 | 86.34 | 29.21 | 1.36 |
| 8| 6829 | 90.06 | 30.37 | 1.40 |
| 9| 6980 | 98.36 | 33.11 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6005 | 30.42 | 10.80 | 0.74 |
| 10 | 10 | 569 | 6174 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1140 | 6515 | 59.10 | 22.22 | 1.07 |
| 10 | 39 | 2218 | 7157 | 98.05 | 37.58 | 1.53 |

