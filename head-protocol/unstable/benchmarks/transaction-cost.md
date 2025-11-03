--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-03 08:46:42.404946015 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.72 | 4.66 | 0.58 |
| 5| 6643 | 18.58 | 5.86 | 0.64 |
| 10| 7646 | 29.14 | 9.19 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1270 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.32 | 9.66 | 0.52 |
| 3 | 169 | 747 | 43.57 | 12.47 | 0.63 |
| 4 | 225 | 858 | 49.68 | 14.37 | 0.69 |
| 5 | 283 | 969 | 55.96 | 16.21 | 0.76 |
| 6 | 340 | 1081 | 75.90 | 21.48 | 0.97 |
| 7 | 394 | 1192 | 87.10 | 24.57 | 1.08 |
| 8 | 450 | 1303 | 99.11 | 27.90 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1801 | 24.29 | 7.69 | 0.48 |
| 2| 1916 | 25.39 | 8.68 | 0.50 |
| 3| 2142 | 29.47 | 10.46 | 0.56 |
| 5| 2327 | 29.93 | 11.94 | 0.58 |
| 10| 3143 | 41.70 | 18.56 | 0.76 |
| 41| 7767 | 99.34 | 55.23 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 626 | 22.54 | 7.30 | 0.41 |
| 2| 769 | 24.31 | 8.46 | 0.44 |
| 3| 908 | 25.10 | 9.32 | 0.46 |
| 5| 1120 | 27.12 | 11.22 | 0.50 |
| 10| 1934 | 38.52 | 17.75 | 0.67 |
| 43| 6921 | 98.82 | 56.55 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 812 | 29.22 | 9.61 | 0.49 |
| 3| 898 | 30.23 | 10.54 | 0.51 |
| 5| 1176 | 36.38 | 13.58 | 0.59 |
| 10| 1903 | 46.21 | 19.66 | 0.75 |
| 35| 5632 | 93.35 | 49.60 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 33.83 | 10.15 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 1005 | 38.55 | 12.81 | 0.60 |
| 5| 1221 | 41.86 | 15.04 | 0.65 |
| 10| 2156 | 56.05 | 22.42 | 0.86 |
| 29| 4922 | 97.80 | 46.66 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.08 | 9.08 | 0.69 |
| 2| 5941 | 36.11 | 12.13 | 0.79 |
| 3| 5900 | 36.97 | 12.25 | 0.80 |
| 4| 6123 | 47.04 | 15.71 | 0.91 |
| 5| 6409 | 61.55 | 20.68 | 1.08 |
| 6| 6577 | 74.55 | 25.17 | 1.22 |
| 7| 6831 | 80.92 | 27.34 | 1.30 |
| 8| 6818 | 87.98 | 29.65 | 1.37 |
| 9| 6760 | 93.14 | 31.25 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6174 | 38.81 | 14.21 | 0.84 |
| 10 | 20 | 1138 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7160 | 99.38 | 38.04 | 1.54 |

