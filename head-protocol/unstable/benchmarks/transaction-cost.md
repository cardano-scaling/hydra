--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-11 11:13:20.801483378 UTC |
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
| 1| 5834 | 10.55 | 3.35 | 0.52 |
| 2| 6038 | 12.91 | 4.10 | 0.55 |
| 3| 6236 | 14.47 | 4.57 | 0.57 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7651 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 170 | 747 | 39.89 | 11.59 | 0.59 |
| 4 | 227 | 862 | 49.73 | 14.36 | 0.69 |
| 5 | 283 | 974 | 58.15 | 16.86 | 0.78 |
| 6 | 338 | 1081 | 64.18 | 18.67 | 0.85 |
| 7 | 393 | 1192 | 76.98 | 22.15 | 0.98 |
| 8 | 449 | 1303 | 91.70 | 26.07 | 1.14 |
| 9 | 506 | 1414 | 92.23 | 26.56 | 1.15 |
| 10 | 560 | 1529 | 97.19 | 28.20 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 23.30 | 7.41 | 0.47 |
| 2| 1882 | 24.77 | 8.48 | 0.49 |
| 3| 2082 | 26.98 | 9.78 | 0.53 |
| 5| 2427 | 32.40 | 12.62 | 0.61 |
| 10| 3122 | 41.04 | 18.36 | 0.75 |
| 39| 7513 | 96.79 | 53.20 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 638 | 22.50 | 7.29 | 0.41 |
| 2| 804 | 25.47 | 8.77 | 0.45 |
| 3| 931 | 27.10 | 9.90 | 0.48 |
| 5| 1217 | 29.58 | 11.93 | 0.53 |
| 10| 2030 | 40.19 | 18.21 | 0.70 |
| 39| 6408 | 98.41 | 53.72 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 27.47 | 8.46 | 0.46 |
| 2| 771 | 28.47 | 9.38 | 0.48 |
| 3| 959 | 30.82 | 10.73 | 0.52 |
| 5| 1294 | 34.97 | 13.23 | 0.59 |
| 10| 2024 | 47.22 | 19.99 | 0.76 |
| 36| 6036 | 98.99 | 51.89 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 33.12 | 9.94 | 0.52 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 896 | 37.24 | 12.41 | 0.58 |
| 5| 1363 | 43.91 | 15.67 | 0.68 |
| 10| 2054 | 54.66 | 22.00 | 0.84 |
| 29| 4908 | 98.15 | 46.75 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 26.92 | 9.04 | 0.69 |
| 2| 5875 | 32.68 | 10.92 | 0.75 |
| 3| 6086 | 44.61 | 15.01 | 0.89 |
| 4| 6263 | 54.93 | 18.51 | 1.00 |
| 5| 6455 | 63.93 | 21.61 | 1.11 |
| 6| 6512 | 73.36 | 24.72 | 1.21 |
| 7| 6628 | 83.37 | 28.02 | 1.32 |
| 8| 6828 | 87.02 | 29.28 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 570 | 6174 | 38.37 | 14.06 | 0.83 |
| 10 | 20 | 1138 | 6512 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1708 | 6855 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2220 | 7159 | 98.93 | 37.88 | 1.54 |

