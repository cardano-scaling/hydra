--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-15 04:48:00.575338298 UTC |
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
| 1| 5836 | 10.72 | 3.41 | 0.52 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6240 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 19.19 | 6.08 | 0.64 |
| 10| 7646 | 29.12 | 9.18 | 0.79 |
| 43| 14282 | 99.49 | 31.12 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 924 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.33 | 9.64 | 0.52 |
| 3 | 170 | 747 | 41.42 | 11.99 | 0.60 |
| 4 | 227 | 858 | 50.99 | 14.66 | 0.71 |
| 5 | 283 | 974 | 57.68 | 16.65 | 0.78 |
| 6 | 339 | 1081 | 64.16 | 18.60 | 0.85 |
| 7 | 395 | 1192 | 82.96 | 23.54 | 1.04 |
| 8 | 449 | 1307 | 86.77 | 24.74 | 1.09 |
| 9 | 504 | 1414 | 91.74 | 26.50 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.37 | 7.71 | 0.48 |
| 2| 1926 | 25.39 | 8.68 | 0.50 |
| 3| 2095 | 28.39 | 10.16 | 0.55 |
| 5| 2435 | 32.10 | 12.54 | 0.61 |
| 10| 3296 | 44.46 | 19.33 | 0.79 |
| 39| 7495 | 96.79 | 53.23 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 22.54 | 7.31 | 0.41 |
| 2| 766 | 24.05 | 8.39 | 0.44 |
| 3| 894 | 25.09 | 9.32 | 0.46 |
| 5| 1252 | 30.06 | 12.06 | 0.54 |
| 10| 2084 | 40.49 | 18.30 | 0.70 |
| 41| 6601 | 99.93 | 55.47 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 29.13 | 8.90 | 0.48 |
| 2| 775 | 30.98 | 10.08 | 0.51 |
| 3| 906 | 30.26 | 10.55 | 0.51 |
| 5| 1260 | 35.38 | 13.35 | 0.59 |
| 10| 2153 | 45.76 | 19.64 | 0.76 |
| 35| 5987 | 97.62 | 50.91 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 1064 | 39.34 | 13.05 | 0.61 |
| 5| 1279 | 42.72 | 15.30 | 0.66 |
| 10| 2096 | 54.89 | 22.05 | 0.84 |
| 29| 4994 | 99.13 | 47.05 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5803 | 27.09 | 9.08 | 0.69 |
| 2| 5946 | 36.01 | 12.11 | 0.79 |
| 3| 6056 | 44.75 | 15.10 | 0.89 |
| 4| 6245 | 52.58 | 17.69 | 0.98 |
| 5| 6531 | 65.28 | 22.01 | 1.12 |
| 6| 6648 | 75.60 | 25.51 | 1.24 |
| 7| 6770 | 83.76 | 28.22 | 1.33 |
| 8| 6867 | 89.23 | 30.05 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 20 | 1140 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 97.61 | 37.43 | 1.52 |

