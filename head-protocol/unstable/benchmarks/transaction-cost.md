--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 04:41:59.458105656 UTC |
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
| 1| 5840 | 10.55 | 3.35 | 0.52 |
| 2| 6035 | 12.32 | 3.89 | 0.54 |
| 3| 6239 | 14.60 | 4.62 | 0.58 |
| 5| 6641 | 18.83 | 5.95 | 0.64 |
| 10| 7651 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 99.08 | 30.97 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 169 | 751 | 41.28 | 11.92 | 0.60 |
| 4 | 226 | 858 | 48.37 | 14.05 | 0.68 |
| 5 | 284 | 969 | 64.74 | 18.38 | 0.85 |
| 6 | 338 | 1085 | 72.10 | 20.58 | 0.93 |
| 7 | 393 | 1192 | 78.47 | 22.46 | 1.00 |
| 8 | 451 | 1303 | 91.31 | 25.88 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 24.29 | 7.69 | 0.48 |
| 2| 1917 | 25.47 | 8.70 | 0.50 |
| 3| 2161 | 28.93 | 10.33 | 0.55 |
| 5| 2385 | 30.96 | 12.23 | 0.59 |
| 10| 3068 | 39.70 | 18.00 | 0.74 |
| 39| 7364 | 93.74 | 52.37 | 1.61 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 641 | 22.57 | 7.32 | 0.41 |
| 2| 801 | 25.20 | 8.72 | 0.45 |
| 3| 945 | 27.10 | 9.90 | 0.48 |
| 5| 1186 | 27.97 | 11.46 | 0.51 |
| 10| 2100 | 40.83 | 18.40 | 0.71 |
| 43| 6779 | 99.56 | 56.77 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.54 | 8.47 | 0.46 |
| 2| 836 | 31.62 | 10.28 | 0.52 |
| 3| 1001 | 31.53 | 10.94 | 0.53 |
| 5| 1304 | 37.70 | 13.98 | 0.61 |
| 10| 1860 | 45.31 | 19.39 | 0.74 |
| 38| 6186 | 99.47 | 53.35 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.83 | 10.15 | 0.53 |
| 2| 803 | 35.92 | 11.40 | 0.56 |
| 3| 1054 | 39.34 | 13.05 | 0.61 |
| 5| 1378 | 44.15 | 15.73 | 0.68 |
| 10| 2067 | 54.25 | 21.85 | 0.84 |
| 28| 4913 | 97.34 | 45.92 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.93 | 7.56 | 0.64 |
| 2| 5976 | 35.99 | 12.08 | 0.79 |
| 3| 6139 | 46.83 | 15.83 | 0.91 |
| 4| 6303 | 56.27 | 18.98 | 1.02 |
| 5| 6398 | 65.12 | 22.01 | 1.12 |
| 6| 6544 | 70.71 | 23.84 | 1.18 |
| 7| 6820 | 84.45 | 28.52 | 1.34 |
| 8| 7121 | 95.10 | 32.23 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 56 | 5867 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6004 | 27.58 | 9.82 | 0.71 |
| 10 | 20 | 1139 | 6513 | 58.84 | 22.14 | 1.07 |
| 10 | 39 | 2220 | 7159 | 98.93 | 37.88 | 1.54 |

