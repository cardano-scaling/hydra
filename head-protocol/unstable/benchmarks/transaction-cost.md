--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-26 09:51:42.726613974 UTC |
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
| 1| 5837 | 10.95 | 3.49 | 0.52 |
| 2| 6042 | 12.41 | 3.92 | 0.54 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7651 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 169 | 747 | 43.92 | 12.57 | 0.63 |
| 4 | 227 | 858 | 52.11 | 14.90 | 0.72 |
| 5 | 283 | 969 | 64.55 | 18.30 | 0.85 |
| 6 | 337 | 1081 | 69.24 | 19.81 | 0.90 |
| 7 | 393 | 1192 | 74.66 | 21.59 | 0.96 |
| 8 | 449 | 1303 | 80.76 | 23.46 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.00 | 7.62 | 0.48 |
| 2| 1882 | 24.77 | 8.48 | 0.49 |
| 3| 2082 | 27.47 | 9.90 | 0.54 |
| 5| 2370 | 30.84 | 12.20 | 0.59 |
| 10| 3254 | 42.64 | 18.83 | 0.77 |
| 41| 7657 | 98.25 | 54.93 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.81 | 7.37 | 0.42 |
| 2| 801 | 25.56 | 8.80 | 0.46 |
| 3| 895 | 24.99 | 9.29 | 0.46 |
| 5| 1313 | 32.20 | 12.65 | 0.56 |
| 10| 1941 | 38.85 | 17.84 | 0.68 |
| 40| 6352 | 93.73 | 53.10 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 26.83 | 8.26 | 0.45 |
| 2| 847 | 29.15 | 9.59 | 0.49 |
| 3| 992 | 31.65 | 10.97 | 0.53 |
| 5| 1248 | 35.01 | 13.24 | 0.58 |
| 10| 2003 | 45.01 | 19.40 | 0.74 |
| 36| 5955 | 97.91 | 51.62 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 33.87 | 10.16 | 0.53 |
| 2| 829 | 35.85 | 11.38 | 0.56 |
| 3| 994 | 38.58 | 12.82 | 0.60 |
| 5| 1201 | 41.89 | 15.05 | 0.65 |
| 10| 2294 | 57.51 | 22.86 | 0.88 |
| 28| 4913 | 98.20 | 46.13 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.58 | 0.64 |
| 2| 5916 | 34.75 | 11.63 | 0.78 |
| 3| 6039 | 41.48 | 13.89 | 0.85 |
| 4| 6162 | 50.39 | 16.86 | 0.95 |
| 5| 6379 | 60.07 | 20.24 | 1.06 |
| 6| 6613 | 74.54 | 25.10 | 1.22 |
| 7| 6525 | 73.70 | 24.78 | 1.21 |
| 8| 6766 | 89.42 | 30.08 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6512 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1705 | 6852 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2219 | 7159 | 99.38 | 38.04 | 1.54 |

