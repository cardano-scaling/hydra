--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-29 04:51:00.208099384 UTC |
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
| 1| 5836 | 11.12 | 3.55 | 0.52 |
| 2| 6037 | 12.75 | 4.04 | 0.55 |
| 3| 6239 | 14.50 | 4.58 | 0.58 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7644 | 29.47 | 9.30 | 0.79 |
| 43| 14281 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 43.80 | 12.53 | 0.63 |
| 4 | 226 | 858 | 51.50 | 14.85 | 0.71 |
| 5 | 283 | 969 | 63.84 | 18.09 | 0.84 |
| 6 | 340 | 1081 | 74.01 | 21.03 | 0.95 |
| 7 | 393 | 1192 | 78.80 | 22.58 | 1.00 |
| 8 | 450 | 1303 | 84.05 | 24.35 | 1.06 |
| 9 | 505 | 1418 | 93.73 | 26.91 | 1.16 |
| 10 | 561 | 1525 | 97.09 | 28.18 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1820 | 24.00 | 7.62 | 0.48 |
| 2| 2007 | 26.58 | 9.01 | 0.52 |
| 3| 2143 | 28.39 | 10.16 | 0.55 |
| 5| 2344 | 30.29 | 12.03 | 0.58 |
| 10| 3152 | 40.82 | 18.31 | 0.75 |
| 41| 7651 | 99.18 | 55.21 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 22.84 | 7.37 | 0.41 |
| 2| 739 | 24.04 | 8.39 | 0.44 |
| 3| 903 | 25.10 | 9.32 | 0.46 |
| 5| 1202 | 29.11 | 11.78 | 0.52 |
| 10| 2003 | 39.18 | 17.95 | 0.68 |
| 40| 6429 | 96.26 | 53.81 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.46 | 8.69 | 0.47 |
| 2| 793 | 30.94 | 10.07 | 0.51 |
| 3| 898 | 30.26 | 10.55 | 0.51 |
| 5| 1172 | 36.38 | 13.58 | 0.59 |
| 10| 2028 | 44.98 | 19.39 | 0.74 |
| 36| 6097 | 98.45 | 51.77 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 33.83 | 10.15 | 0.53 |
| 2| 807 | 35.92 | 11.40 | 0.56 |
| 3| 958 | 38.25 | 12.72 | 0.59 |
| 5| 1290 | 43.25 | 15.47 | 0.67 |
| 10| 2019 | 54.00 | 21.79 | 0.83 |
| 29| 4800 | 97.18 | 46.46 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5786 | 27.09 | 9.09 | 0.69 |
| 2| 5936 | 35.83 | 12.03 | 0.79 |
| 3| 6141 | 45.73 | 15.40 | 0.90 |
| 4| 6309 | 55.35 | 18.68 | 1.01 |
| 5| 6381 | 62.58 | 21.03 | 1.09 |
| 6| 6674 | 75.68 | 25.56 | 1.24 |
| 7| 6733 | 83.86 | 28.28 | 1.33 |
| 8| 6760 | 91.67 | 30.85 | 1.41 |
| 9| 7059 | 99.45 | 33.44 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6003 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 38 | 2164 | 7127 | 96.00 | 36.77 | 1.50 |

