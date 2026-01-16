--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-16 20:18:02.903486456 UTC |
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
| 2| 6038 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.79 | 4.69 | 0.58 |
| 5| 6638 | 19.08 | 6.04 | 0.64 |
| 10| 7644 | 29.47 | 9.30 | 0.79 |
| 43| 14286 | 99.32 | 31.06 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 170 | 747 | 41.29 | 11.96 | 0.60 |
| 4 | 226 | 862 | 52.14 | 14.96 | 0.72 |
| 5 | 284 | 969 | 58.31 | 16.84 | 0.79 |
| 6 | 338 | 1081 | 64.69 | 18.83 | 0.86 |
| 7 | 393 | 1192 | 86.82 | 24.46 | 1.08 |
| 8 | 449 | 1307 | 80.52 | 23.35 | 1.03 |
| 9 | 505 | 1414 | 96.26 | 27.57 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 24.00 | 7.62 | 0.48 |
| 2| 1996 | 26.47 | 8.98 | 0.52 |
| 3| 2011 | 25.98 | 9.50 | 0.52 |
| 5| 2318 | 29.97 | 11.95 | 0.58 |
| 10| 3271 | 43.66 | 19.11 | 0.78 |
| 41| 7575 | 97.06 | 54.62 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 642 | 22.81 | 7.37 | 0.42 |
| 2| 743 | 24.31 | 8.47 | 0.44 |
| 3| 952 | 26.84 | 9.83 | 0.48 |
| 5| 1179 | 30.04 | 12.05 | 0.53 |
| 10| 2070 | 42.22 | 18.78 | 0.72 |
| 40| 6457 | 95.97 | 53.73 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.17 | 8.91 | 0.48 |
| 2| 819 | 31.58 | 10.27 | 0.52 |
| 3| 869 | 32.05 | 11.02 | 0.53 |
| 5| 1268 | 34.97 | 13.23 | 0.58 |
| 10| 2032 | 44.79 | 19.34 | 0.74 |
| 36| 6027 | 98.94 | 51.91 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 698 | 33.83 | 10.15 | 0.53 |
| 2| 764 | 35.17 | 11.17 | 0.55 |
| 3| 934 | 37.84 | 12.60 | 0.59 |
| 5| 1261 | 42.64 | 15.28 | 0.66 |
| 10| 2091 | 54.72 | 22.01 | 0.84 |
| 28| 4822 | 97.90 | 46.06 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5799 | 27.00 | 9.06 | 0.69 |
| 2| 5937 | 35.96 | 12.11 | 0.79 |
| 3| 6233 | 46.83 | 15.85 | 0.92 |
| 4| 6188 | 52.47 | 17.63 | 0.97 |
| 5| 6398 | 63.02 | 21.18 | 1.09 |
| 6| 6500 | 70.08 | 23.57 | 1.17 |
| 7| 6625 | 76.64 | 25.72 | 1.25 |
| 8| 6933 | 94.56 | 31.95 | 1.45 |
| 9| 7049 | 99.89 | 33.76 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.85 | 7.43 | 0.64 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 39 | 2220 | 7159 | 98.49 | 37.73 | 1.53 |

