--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-26 14:13:42.81180654 UTC |
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
| 1| 5837 | 10.26 | 3.25 | 0.51 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6238 | 14.48 | 4.58 | 0.57 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14279 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10079 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 170 | 751 | 41.51 | 12.00 | 0.61 |
| 4 | 228 | 858 | 48.61 | 14.14 | 0.68 |
| 5 | 284 | 969 | 62.81 | 17.88 | 0.83 |
| 6 | 338 | 1081 | 69.90 | 19.97 | 0.91 |
| 7 | 396 | 1192 | 74.40 | 21.44 | 0.96 |
| 8 | 450 | 1303 | 99.10 | 27.85 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 23.92 | 7.60 | 0.48 |
| 2| 1984 | 26.55 | 9.00 | 0.52 |
| 3| 2118 | 28.27 | 10.13 | 0.54 |
| 5| 2407 | 31.72 | 12.43 | 0.60 |
| 10| 3225 | 43.06 | 18.93 | 0.78 |
| 39| 7584 | 97.74 | 53.50 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.84 | 7.38 | 0.42 |
| 2| 719 | 22.60 | 7.95 | 0.42 |
| 3| 976 | 26.02 | 9.58 | 0.47 |
| 5| 1302 | 31.02 | 12.33 | 0.55 |
| 10| 2036 | 39.55 | 18.03 | 0.69 |
| 42| 6774 | 98.29 | 55.73 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.17 | 8.91 | 0.48 |
| 2| 855 | 29.90 | 9.82 | 0.50 |
| 3| 917 | 32.64 | 11.21 | 0.54 |
| 5| 1327 | 38.38 | 14.19 | 0.62 |
| 10| 1971 | 44.11 | 19.13 | 0.73 |
| 37| 5968 | 98.38 | 52.38 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 1023 | 39.26 | 13.03 | 0.61 |
| 5| 1252 | 42.68 | 15.29 | 0.66 |
| 10| 2119 | 55.55 | 22.26 | 0.85 |
| 29| 4995 | 99.52 | 47.14 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 26.92 | 9.05 | 0.69 |
| 2| 5917 | 32.52 | 10.87 | 0.75 |
| 3| 6076 | 45.08 | 15.12 | 0.89 |
| 4| 6304 | 54.83 | 18.53 | 1.00 |
| 5| 6344 | 60.62 | 20.35 | 1.07 |
| 6| 6786 | 76.14 | 25.76 | 1.25 |
| 7| 6793 | 80.84 | 27.21 | 1.30 |
| 8| 6956 | 94.12 | 31.75 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 10 | 568 | 6172 | 40.83 | 14.90 | 0.86 |
| 10 | 20 | 1140 | 6515 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1708 | 6854 | 81.11 | 30.83 | 1.33 |
| 10 | 39 | 2220 | 7160 | 98.93 | 37.88 | 1.54 |

