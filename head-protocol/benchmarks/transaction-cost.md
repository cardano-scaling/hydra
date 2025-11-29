--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-29 10:03:29.740883871 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.32 | 3.89 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 19.10 | 6.05 | 0.64 |
| 10| 7651 | 29.31 | 9.25 | 0.79 |
| 43| 14285 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 170 | 747 | 42.73 | 12.29 | 0.62 |
| 4 | 227 | 858 | 50.97 | 14.63 | 0.71 |
| 5 | 282 | 969 | 56.20 | 16.33 | 0.76 |
| 6 | 338 | 1081 | 68.12 | 19.55 | 0.89 |
| 7 | 395 | 1192 | 72.73 | 21.09 | 0.94 |
| 8 | 452 | 1303 | 92.09 | 26.12 | 1.14 |
| 10 | 562 | 1525 | 96.72 | 28.09 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 23.92 | 7.60 | 0.48 |
| 2| 1925 | 25.88 | 8.79 | 0.51 |
| 3| 2098 | 28.02 | 10.08 | 0.54 |
| 5| 2318 | 30.26 | 12.02 | 0.58 |
| 10| 3056 | 39.00 | 17.79 | 0.73 |
| 41| 7564 | 98.35 | 54.92 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.84 | 7.37 | 0.42 |
| 2| 766 | 24.05 | 8.39 | 0.44 |
| 3| 831 | 24.09 | 9.04 | 0.45 |
| 5| 1202 | 29.08 | 11.77 | 0.52 |
| 10| 1953 | 38.66 | 17.78 | 0.68 |
| 39| 6516 | 99.64 | 54.08 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 988 | 31.69 | 10.98 | 0.53 |
| 5| 1256 | 37.14 | 13.81 | 0.61 |
| 10| 1991 | 47.26 | 19.99 | 0.76 |
| 35| 5663 | 93.13 | 49.52 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 683 | 33.87 | 10.16 | 0.53 |
| 2| 806 | 35.85 | 11.38 | 0.56 |
| 3| 938 | 37.91 | 12.62 | 0.59 |
| 5| 1284 | 43.36 | 15.49 | 0.67 |
| 10| 2135 | 55.59 | 22.29 | 0.85 |
| 29| 4855 | 98.73 | 46.89 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5819 | 27.09 | 9.09 | 0.69 |
| 2| 5993 | 36.85 | 12.43 | 0.80 |
| 3| 6161 | 45.55 | 15.38 | 0.90 |
| 4| 6191 | 51.21 | 17.20 | 0.96 |
| 5| 6384 | 60.79 | 20.43 | 1.07 |
| 6| 6580 | 74.10 | 25.00 | 1.22 |
| 7| 6683 | 79.59 | 26.80 | 1.28 |
| 8| 6870 | 92.85 | 31.27 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 283 | 6002 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1138 | 6512 | 60.35 | 22.66 | 1.09 |
| 10 | 30 | 1706 | 6852 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2217 | 7157 | 99.38 | 38.04 | 1.54 |

