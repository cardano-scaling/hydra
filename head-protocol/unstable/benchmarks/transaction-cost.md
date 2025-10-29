--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-29 12:23:37.313845976 UTC |
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
| 1| 5836 | 10.40 | 3.30 | 0.51 |
| 2| 6038 | 12.63 | 4.00 | 0.55 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6643 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.21 | 9.21 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1284 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.30 | 9.40 | 0.51 |
| 3 | 169 | 747 | 42.51 | 12.22 | 0.61 |
| 4 | 226 | 858 | 51.28 | 14.75 | 0.71 |
| 5 | 284 | 969 | 55.67 | 16.13 | 0.76 |
| 6 | 338 | 1081 | 66.11 | 19.14 | 0.87 |
| 7 | 395 | 1192 | 84.40 | 23.84 | 1.06 |
| 8 | 450 | 1307 | 97.19 | 27.44 | 1.19 |
| 9 | 504 | 1414 | 88.37 | 25.63 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1803 | 24.00 | 7.62 | 0.48 |
| 2| 1883 | 24.44 | 8.41 | 0.49 |
| 3| 2059 | 27.02 | 9.79 | 0.53 |
| 5| 2319 | 29.93 | 11.94 | 0.58 |
| 10| 3097 | 39.66 | 17.99 | 0.74 |
| 37| 7041 | 91.30 | 50.32 | 1.56 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.57 | 7.31 | 0.41 |
| 2| 697 | 22.58 | 7.94 | 0.42 |
| 3| 933 | 26.94 | 9.85 | 0.48 |
| 5| 1276 | 31.35 | 12.41 | 0.55 |
| 10| 2033 | 39.42 | 18.00 | 0.69 |
| 39| 6587 | 99.56 | 54.10 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 658 | 29.17 | 8.91 | 0.48 |
| 2| 770 | 28.51 | 9.39 | 0.48 |
| 3| 949 | 30.94 | 10.75 | 0.52 |
| 5| 1332 | 38.52 | 14.23 | 0.62 |
| 10| 1886 | 45.94 | 19.59 | 0.75 |
| 35| 5810 | 95.53 | 50.22 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 33.83 | 10.15 | 0.53 |
| 2| 814 | 35.89 | 11.39 | 0.56 |
| 3| 934 | 37.84 | 12.60 | 0.59 |
| 5| 1355 | 43.17 | 15.45 | 0.67 |
| 10| 2062 | 54.02 | 21.80 | 0.83 |
| 30| 4843 | 98.40 | 47.39 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.97 | 7.57 | 0.64 |
| 2| 5943 | 35.96 | 12.08 | 0.79 |
| 3| 6186 | 45.54 | 15.38 | 0.90 |
| 4| 6202 | 53.88 | 18.14 | 0.99 |
| 5| 6508 | 65.65 | 22.20 | 1.13 |
| 6| 6534 | 71.42 | 24.04 | 1.19 |
| 7| 6697 | 79.98 | 26.87 | 1.28 |
| 8| 7011 | 95.46 | 32.26 | 1.46 |
| 9| 7007 | 98.24 | 33.01 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 39 | 2222 | 7161 | 97.61 | 37.43 | 1.52 |

