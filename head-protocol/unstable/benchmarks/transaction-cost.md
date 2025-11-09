--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-09 04:47:37.213432583 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6037 | 12.78 | 4.06 | 0.55 |
| 3| 6236 | 14.84 | 4.71 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 29.30 | 9.24 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1273 | 6.41 | 3.60 | 0.28 |
| 10| 2166 | 12.13 | 7.25 | 0.40 |
| 54| 10055 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 170 | 747 | 40.51 | 11.80 | 0.60 |
| 4 | 227 | 858 | 53.69 | 15.31 | 0.73 |
| 5 | 283 | 969 | 64.53 | 18.35 | 0.85 |
| 6 | 340 | 1081 | 71.54 | 20.40 | 0.92 |
| 7 | 395 | 1192 | 83.28 | 23.70 | 1.05 |
| 8 | 450 | 1303 | 92.40 | 26.24 | 1.14 |
| 9 | 505 | 1414 | 96.82 | 27.71 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1807 | 24.37 | 7.71 | 0.48 |
| 2| 1981 | 26.87 | 9.07 | 0.52 |
| 3| 2088 | 26.94 | 9.77 | 0.53 |
| 5| 2438 | 32.41 | 12.62 | 0.61 |
| 10| 3134 | 40.96 | 18.34 | 0.75 |
| 39| 7595 | 99.29 | 53.86 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 796 | 25.20 | 8.72 | 0.45 |
| 3| 974 | 26.95 | 9.86 | 0.48 |
| 5| 1093 | 27.04 | 11.20 | 0.50 |
| 10| 2170 | 42.95 | 18.99 | 0.73 |
| 40| 6451 | 95.06 | 53.47 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 736 | 30.23 | 9.85 | 0.50 |
| 3| 911 | 32.72 | 11.23 | 0.54 |
| 5| 1232 | 34.26 | 13.02 | 0.58 |
| 10| 2173 | 46.48 | 19.84 | 0.76 |
| 35| 5650 | 98.20 | 50.90 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 1008 | 38.62 | 12.83 | 0.60 |
| 5| 1329 | 43.20 | 15.45 | 0.67 |
| 10| 1968 | 53.42 | 21.61 | 0.82 |
| 30| 4817 | 97.11 | 47.07 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 26.96 | 9.06 | 0.69 |
| 2| 5946 | 36.03 | 12.10 | 0.79 |
| 3| 6014 | 44.16 | 14.79 | 0.88 |
| 4| 6225 | 53.95 | 18.15 | 0.99 |
| 5| 6517 | 66.29 | 22.44 | 1.13 |
| 6| 6427 | 70.19 | 23.55 | 1.17 |
| 7| 6622 | 76.98 | 25.87 | 1.25 |
| 8| 6909 | 94.10 | 31.86 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 569 | 6174 | 39.69 | 14.52 | 0.85 |
| 10 | 20 | 1138 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1710 | 6857 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2221 | 7160 | 98.05 | 37.58 | 1.53 |

