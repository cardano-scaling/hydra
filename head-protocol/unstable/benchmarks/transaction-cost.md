--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-13 11:25:52.258053148 UTC |
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
| 1| 5841 | 10.78 | 3.43 | 0.52 |
| 2| 6035 | 12.78 | 4.06 | 0.55 |
| 3| 6239 | 15.05 | 4.78 | 0.58 |
| 5| 6640 | 19.08 | 6.04 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 40.22 | 11.69 | 0.59 |
| 4 | 227 | 862 | 49.69 | 14.37 | 0.69 |
| 5 | 281 | 969 | 62.67 | 17.82 | 0.83 |
| 6 | 339 | 1081 | 66.24 | 19.13 | 0.87 |
| 7 | 394 | 1192 | 81.20 | 23.20 | 1.03 |
| 8 | 449 | 1303 | 85.44 | 24.53 | 1.07 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.37 | 7.71 | 0.48 |
| 2| 1924 | 25.47 | 8.70 | 0.50 |
| 3| 2105 | 28.31 | 10.14 | 0.54 |
| 5| 2536 | 34.63 | 13.24 | 0.64 |
| 10| 3065 | 39.75 | 18.01 | 0.74 |
| 38| 7374 | 94.84 | 52.01 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.84 | 7.40 | 0.42 |
| 2| 824 | 25.35 | 8.74 | 0.45 |
| 3| 875 | 25.82 | 9.54 | 0.47 |
| 5| 1160 | 28.39 | 11.61 | 0.51 |
| 10| 1969 | 38.59 | 17.76 | 0.68 |
| 41| 6713 | 99.05 | 55.26 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 665 | 29.17 | 8.91 | 0.48 |
| 2| 770 | 28.51 | 9.39 | 0.48 |
| 3| 948 | 30.94 | 10.75 | 0.52 |
| 5| 1348 | 38.44 | 14.21 | 0.62 |
| 10| 2055 | 47.97 | 20.21 | 0.77 |
| 36| 6108 | 99.47 | 52.09 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 33.83 | 10.15 | 0.53 |
| 2| 819 | 35.88 | 11.39 | 0.56 |
| 3| 943 | 37.95 | 12.63 | 0.59 |
| 5| 1382 | 43.88 | 15.66 | 0.68 |
| 10| 1967 | 53.28 | 21.57 | 0.82 |
| 29| 4970 | 99.39 | 47.11 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 26.97 | 9.05 | 0.69 |
| 2| 5995 | 37.01 | 12.45 | 0.80 |
| 3| 6108 | 45.03 | 15.12 | 0.89 |
| 4| 6168 | 50.23 | 16.88 | 0.95 |
| 5| 6407 | 62.97 | 21.12 | 1.09 |
| 6| 6684 | 74.37 | 25.08 | 1.23 |
| 7| 6716 | 80.49 | 27.12 | 1.29 |
| 8| 6945 | 92.90 | 31.26 | 1.43 |
| 9| 7099 | 96.93 | 32.69 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 283 | 6003 | 30.42 | 10.80 | 0.74 |
| 10 | 20 | 1139 | 6513 | 60.17 | 22.59 | 1.09 |
| 10 | 40 | 2277 | 7194 | 99.66 | 38.24 | 1.55 |

