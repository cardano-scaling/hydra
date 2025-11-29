--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-29 04:49:12.990894624 UTC |
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
| 2| 6041 | 12.46 | 3.94 | 0.55 |
| 3| 6238 | 14.98 | 4.75 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7644 | 29.30 | 9.24 | 0.79 |
| 43| 14279 | 99.14 | 30.99 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.30 | 9.40 | 0.51 |
| 3 | 171 | 751 | 43.61 | 12.48 | 0.63 |
| 4 | 226 | 858 | 48.31 | 14.01 | 0.68 |
| 5 | 282 | 969 | 59.67 | 17.13 | 0.80 |
| 6 | 339 | 1081 | 71.44 | 20.37 | 0.92 |
| 7 | 394 | 1192 | 72.18 | 20.99 | 0.94 |
| 8 | 449 | 1303 | 96.46 | 27.17 | 1.18 |
| 9 | 504 | 1414 | 93.28 | 26.80 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1806 | 24.37 | 7.71 | 0.48 |
| 2| 1882 | 24.40 | 8.39 | 0.49 |
| 3| 2124 | 28.55 | 10.20 | 0.55 |
| 5| 2386 | 31.26 | 12.30 | 0.60 |
| 10| 3037 | 38.74 | 17.73 | 0.72 |
| 41| 7653 | 97.01 | 54.60 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.53 | 7.30 | 0.41 |
| 2| 797 | 25.52 | 8.78 | 0.45 |
| 3| 895 | 25.06 | 9.31 | 0.46 |
| 5| 1216 | 29.99 | 12.03 | 0.53 |
| 10| 1877 | 37.53 | 17.48 | 0.66 |
| 41| 6513 | 96.04 | 54.40 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 26.79 | 8.25 | 0.45 |
| 2| 782 | 30.91 | 10.06 | 0.51 |
| 3| 1020 | 34.15 | 11.66 | 0.56 |
| 5| 1241 | 37.10 | 13.80 | 0.60 |
| 10| 2157 | 46.05 | 19.73 | 0.76 |
| 36| 5865 | 94.91 | 50.72 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.12 | 9.94 | 0.52 |
| 2| 823 | 35.92 | 11.40 | 0.56 |
| 3| 938 | 37.95 | 12.63 | 0.59 |
| 5| 1212 | 41.89 | 15.05 | 0.65 |
| 10| 2019 | 53.98 | 21.79 | 0.83 |
| 30| 4779 | 97.24 | 47.08 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5821 | 26.92 | 9.04 | 0.69 |
| 2| 5935 | 35.87 | 12.05 | 0.79 |
| 3| 5996 | 43.89 | 14.70 | 0.87 |
| 4| 6287 | 52.75 | 17.76 | 0.98 |
| 5| 6451 | 65.13 | 21.97 | 1.12 |
| 6| 6537 | 70.35 | 23.70 | 1.18 |
| 7| 6689 | 80.47 | 27.08 | 1.29 |
| 8| 7106 | 96.20 | 32.50 | 1.47 |
| 9| 7032 | 99.27 | 33.42 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 37.74 | 13.85 | 0.83 |
| 10 | 20 | 1139 | 6514 | 59.73 | 22.44 | 1.08 |
| 10 | 39 | 2220 | 7160 | 98.49 | 37.73 | 1.53 |

