--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-18 19:50:18.411016451 UTC |
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
| 1| 5840 | 10.93 | 3.49 | 0.52 |
| 2| 6035 | 12.32 | 3.89 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10066 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 171 | 747 | 41.12 | 11.90 | 0.60 |
| 4 | 227 | 858 | 52.44 | 15.00 | 0.72 |
| 5 | 282 | 969 | 56.05 | 16.26 | 0.76 |
| 6 | 339 | 1081 | 68.32 | 19.67 | 0.89 |
| 7 | 393 | 1192 | 74.15 | 21.38 | 0.96 |
| 8 | 450 | 1303 | 91.59 | 25.95 | 1.13 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.00 | 7.62 | 0.48 |
| 2| 1882 | 24.80 | 8.49 | 0.49 |
| 3| 2053 | 27.27 | 9.85 | 0.53 |
| 5| 2384 | 30.88 | 12.21 | 0.59 |
| 10| 3315 | 42.78 | 18.86 | 0.78 |
| 38| 7532 | 97.01 | 52.58 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.53 | 7.31 | 0.41 |
| 2| 768 | 24.28 | 8.45 | 0.44 |
| 3| 919 | 24.99 | 9.29 | 0.46 |
| 5| 1092 | 27.15 | 11.23 | 0.50 |
| 10| 2066 | 39.57 | 18.04 | 0.69 |
| 41| 6672 | 97.83 | 54.90 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 659 | 29.09 | 8.89 | 0.48 |
| 2| 837 | 29.26 | 9.62 | 0.49 |
| 3| 924 | 32.76 | 11.24 | 0.54 |
| 5| 1366 | 38.44 | 14.21 | 0.62 |
| 10| 2011 | 47.17 | 19.97 | 0.76 |
| 35| 5751 | 99.10 | 51.16 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.79 | 10.15 | 0.53 |
| 2| 806 | 35.89 | 11.39 | 0.56 |
| 3| 997 | 38.59 | 12.82 | 0.60 |
| 5| 1251 | 42.53 | 15.25 | 0.66 |
| 10| 1940 | 53.50 | 21.63 | 0.82 |
| 28| 4917 | 99.63 | 46.57 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 23.01 | 7.58 | 0.64 |
| 2| 5961 | 35.99 | 12.08 | 0.79 |
| 3| 6014 | 41.60 | 13.92 | 0.85 |
| 4| 6260 | 51.16 | 17.21 | 0.96 |
| 5| 6366 | 60.25 | 20.25 | 1.06 |
| 6| 6506 | 69.90 | 23.53 | 1.17 |
| 7| 6881 | 85.85 | 29.09 | 1.36 |
| 8| 6880 | 93.54 | 31.53 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1708 | 6854 | 80.48 | 30.61 | 1.32 |
| 10 | 37 | 2108 | 7093 | 95.28 | 36.42 | 1.49 |

