--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-16 04:50:55.477300023 UTC |
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
| 1| 5837 | 10.40 | 3.30 | 0.51 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6242 | 14.86 | 4.71 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.80 | 9.07 | 0.78 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2169 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 43.56 | 12.48 | 0.63 |
| 4 | 227 | 862 | 53.77 | 15.32 | 0.73 |
| 5 | 284 | 969 | 64.68 | 18.36 | 0.85 |
| 6 | 341 | 1081 | 65.59 | 18.90 | 0.86 |
| 7 | 392 | 1196 | 72.77 | 21.14 | 0.94 |
| 8 | 451 | 1303 | 84.95 | 24.36 | 1.07 |
| 9 | 506 | 1414 | 89.65 | 26.05 | 1.12 |
| 10 | 561 | 1529 | 97.33 | 28.24 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.29 | 7.69 | 0.48 |
| 2| 1932 | 25.92 | 8.80 | 0.51 |
| 3| 2073 | 27.32 | 9.86 | 0.53 |
| 5| 2481 | 32.41 | 12.62 | 0.61 |
| 10| 3230 | 42.39 | 18.76 | 0.77 |
| 41| 7647 | 98.16 | 54.87 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.81 | 7.37 | 0.42 |
| 2| 739 | 23.65 | 8.24 | 0.43 |
| 3| 943 | 26.59 | 9.76 | 0.48 |
| 5| 1159 | 27.96 | 11.45 | 0.51 |
| 10| 1981 | 38.69 | 17.79 | 0.68 |
| 41| 6651 | 97.90 | 54.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 799 | 30.91 | 10.06 | 0.51 |
| 3| 957 | 33.39 | 11.44 | 0.54 |
| 5| 1248 | 34.93 | 13.22 | 0.58 |
| 10| 1850 | 45.19 | 19.36 | 0.74 |
| 36| 5997 | 97.07 | 51.37 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.15 | 0.53 |
| 2| 808 | 35.92 | 11.40 | 0.56 |
| 3| 1068 | 39.30 | 13.04 | 0.61 |
| 5| 1318 | 43.39 | 15.50 | 0.67 |
| 10| 2071 | 55.18 | 22.14 | 0.85 |
| 29| 4875 | 97.63 | 46.60 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 27.08 | 9.08 | 0.69 |
| 2| 5937 | 35.96 | 12.09 | 0.79 |
| 3| 6084 | 44.64 | 15.03 | 0.89 |
| 4| 6253 | 55.07 | 18.52 | 1.00 |
| 5| 6477 | 65.50 | 22.09 | 1.12 |
| 6| 6731 | 76.20 | 25.72 | 1.25 |
| 7| 6833 | 86.38 | 29.20 | 1.36 |
| 8| 6660 | 83.93 | 28.14 | 1.32 |
| 9| 7012 | 99.92 | 33.67 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.38 | 6.48 | 0.61 |
| 10 | 5 | 284 | 6003 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6174 | 37.74 | 13.85 | 0.83 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6854 | 80.67 | 30.67 | 1.32 |
| 10 | 39 | 2223 | 7162 | 98.24 | 37.65 | 1.53 |

