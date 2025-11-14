--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-14 04:49:30.316813661 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.72 | 4.66 | 0.58 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10071 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 171 | 747 | 41.39 | 11.95 | 0.60 |
| 4 | 226 | 858 | 49.74 | 14.38 | 0.69 |
| 5 | 282 | 974 | 63.94 | 18.12 | 0.84 |
| 6 | 338 | 1081 | 67.62 | 19.42 | 0.88 |
| 7 | 396 | 1192 | 82.45 | 23.41 | 1.04 |
| 8 | 448 | 1303 | 97.95 | 27.43 | 1.20 |
| 10 | 561 | 1525 | 97.28 | 28.22 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 23.92 | 7.60 | 0.48 |
| 2| 1882 | 24.44 | 8.41 | 0.49 |
| 3| 2110 | 28.43 | 10.17 | 0.55 |
| 5| 2317 | 30.42 | 12.06 | 0.58 |
| 10| 3128 | 39.78 | 18.02 | 0.74 |
| 40| 7574 | 98.17 | 54.26 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 637 | 22.80 | 7.39 | 0.42 |
| 2| 792 | 25.47 | 8.78 | 0.45 |
| 3| 830 | 24.09 | 9.05 | 0.45 |
| 5| 1242 | 30.19 | 12.08 | 0.54 |
| 10| 1896 | 37.58 | 17.49 | 0.66 |
| 39| 6293 | 92.90 | 52.18 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 29.13 | 8.90 | 0.48 |
| 2| 855 | 29.94 | 9.83 | 0.50 |
| 3| 922 | 32.80 | 11.25 | 0.54 |
| 5| 1202 | 34.26 | 13.02 | 0.57 |
| 10| 2047 | 45.38 | 19.51 | 0.75 |
| 37| 5901 | 96.18 | 51.70 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 33.83 | 10.16 | 0.53 |
| 2| 845 | 36.64 | 11.62 | 0.57 |
| 3| 962 | 37.91 | 12.62 | 0.59 |
| 5| 1253 | 42.68 | 15.29 | 0.66 |
| 10| 1991 | 53.23 | 21.56 | 0.82 |
| 28| 4701 | 94.30 | 44.99 | 1.44 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5821 | 27.04 | 9.09 | 0.69 |
| 2| 6004 | 36.81 | 12.40 | 0.80 |
| 3| 6147 | 45.53 | 15.37 | 0.90 |
| 4| 6325 | 54.68 | 18.46 | 1.00 |
| 5| 6416 | 63.60 | 21.41 | 1.10 |
| 6| 6486 | 69.55 | 23.34 | 1.17 |
| 7| 6983 | 87.60 | 29.70 | 1.38 |
| 8| 7092 | 96.29 | 32.59 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6004 | 28.65 | 10.19 | 0.72 |
| 10 | 10 | 569 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1708 | 6854 | 80.92 | 30.76 | 1.33 |
| 10 | 40 | 2274 | 7191 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2220 | 7159 | 98.49 | 37.73 | 1.53 |

