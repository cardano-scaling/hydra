--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-27 04:40:29.507357725 UTC |
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
| 1| 5837 | 10.93 | 3.49 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6243 | 14.29 | 4.51 | 0.57 |
| 5| 6646 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 29.14 | 9.19 | 0.79 |
| 43| 14281 | 98.66 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 171 | 747 | 43.91 | 12.57 | 0.63 |
| 4 | 228 | 858 | 52.02 | 14.92 | 0.72 |
| 5 | 285 | 969 | 64.13 | 18.19 | 0.84 |
| 6 | 338 | 1081 | 72.95 | 20.70 | 0.94 |
| 7 | 393 | 1192 | 74.55 | 21.52 | 0.96 |
| 8 | 449 | 1303 | 90.26 | 25.74 | 1.12 |
| 9 | 505 | 1414 | 97.44 | 27.97 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 23.92 | 7.60 | 0.48 |
| 2| 2015 | 26.47 | 8.98 | 0.52 |
| 3| 2080 | 26.98 | 9.78 | 0.53 |
| 5| 2321 | 29.84 | 11.92 | 0.58 |
| 10| 3082 | 39.00 | 17.79 | 0.73 |
| 40| 7621 | 98.23 | 54.26 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 611 | 22.80 | 7.38 | 0.41 |
| 2| 707 | 22.58 | 7.96 | 0.42 |
| 3| 928 | 25.56 | 9.48 | 0.47 |
| 5| 1262 | 31.05 | 12.35 | 0.55 |
| 10| 1990 | 39.48 | 18.02 | 0.69 |
| 40| 6497 | 95.81 | 53.71 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 697 | 27.47 | 8.46 | 0.46 |
| 2| 909 | 29.86 | 9.81 | 0.50 |
| 3| 1070 | 32.24 | 11.16 | 0.54 |
| 5| 1240 | 36.95 | 13.76 | 0.60 |
| 10| 2024 | 44.60 | 19.27 | 0.74 |
| 35| 5687 | 99.37 | 51.19 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 33.79 | 10.15 | 0.53 |
| 2| 802 | 35.89 | 11.39 | 0.56 |
| 3| 1041 | 39.34 | 13.05 | 0.61 |
| 5| 1216 | 41.97 | 15.07 | 0.65 |
| 10| 2068 | 54.59 | 21.98 | 0.84 |
| 30| 4842 | 98.42 | 47.42 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5788 | 27.09 | 9.10 | 0.69 |
| 2| 5924 | 34.87 | 11.67 | 0.78 |
| 3| 6164 | 47.01 | 15.88 | 0.92 |
| 4| 6275 | 55.27 | 18.61 | 1.01 |
| 5| 6552 | 66.32 | 22.39 | 1.14 |
| 6| 6441 | 68.34 | 22.90 | 1.15 |
| 7| 6776 | 84.62 | 28.56 | 1.34 |
| 8| 6904 | 89.71 | 30.29 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6513 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1704 | 6850 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2222 | 7161 | 98.49 | 37.73 | 1.53 |

