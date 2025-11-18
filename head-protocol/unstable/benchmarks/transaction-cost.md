--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-18 04:50:12.355294173 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7650 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 99.25 | 31.03 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10090 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 640 | 33.33 | 9.64 | 0.52 |
| 3 | 169 | 747 | 41.12 | 11.88 | 0.60 |
| 4 | 227 | 858 | 52.59 | 15.06 | 0.72 |
| 5 | 283 | 969 | 57.46 | 16.59 | 0.78 |
| 6 | 338 | 1081 | 68.15 | 19.59 | 0.89 |
| 7 | 394 | 1192 | 84.35 | 23.87 | 1.06 |
| 8 | 450 | 1303 | 87.15 | 24.89 | 1.09 |
| 10 | 560 | 1525 | 97.02 | 28.10 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1802 | 24.29 | 7.69 | 0.48 |
| 2| 1925 | 25.47 | 8.70 | 0.50 |
| 3| 2122 | 27.86 | 10.03 | 0.54 |
| 5| 2381 | 31.53 | 12.37 | 0.60 |
| 10| 3110 | 40.08 | 18.11 | 0.74 |
| 42| 7642 | 96.26 | 55.05 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 649 | 22.54 | 7.31 | 0.41 |
| 2| 797 | 24.28 | 8.45 | 0.44 |
| 3| 912 | 25.72 | 9.52 | 0.47 |
| 5| 1230 | 29.98 | 12.04 | 0.53 |
| 10| 1996 | 41.31 | 18.51 | 0.71 |
| 40| 6374 | 96.09 | 53.76 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 27.54 | 8.47 | 0.46 |
| 2| 813 | 29.26 | 9.62 | 0.49 |
| 3| 903 | 30.19 | 10.53 | 0.51 |
| 5| 1238 | 37.10 | 13.80 | 0.60 |
| 10| 1865 | 42.95 | 18.76 | 0.71 |
| 36| 6137 | 99.38 | 52.06 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 668 | 33.87 | 10.16 | 0.53 |
| 2| 815 | 35.89 | 11.39 | 0.56 |
| 3| 896 | 37.13 | 12.38 | 0.58 |
| 5| 1277 | 42.49 | 15.24 | 0.66 |
| 10| 2018 | 53.87 | 21.76 | 0.83 |
| 30| 4966 | 99.50 | 47.78 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.57 | 0.64 |
| 2| 5908 | 35.95 | 12.06 | 0.79 |
| 3| 6064 | 44.90 | 15.05 | 0.89 |
| 4| 6258 | 55.26 | 18.62 | 1.01 |
| 5| 6458 | 65.01 | 21.96 | 1.12 |
| 6| 6381 | 63.72 | 21.36 | 1.10 |
| 7| 6917 | 86.43 | 29.25 | 1.36 |
| 8| 6904 | 92.95 | 31.37 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.96 | 7.13 | 0.63 |
| 10 | 10 | 570 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 30 | 1707 | 6854 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2217 | 7156 | 97.61 | 37.43 | 1.52 |

