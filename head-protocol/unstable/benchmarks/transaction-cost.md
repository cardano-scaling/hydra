--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-25 10:25:34.939483921 UTC |
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
| 1| 5836 | 10.76 | 3.42 | 0.52 |
| 2| 6042 | 13.01 | 4.14 | 0.55 |
| 3| 6236 | 14.72 | 4.66 | 0.58 |
| 5| 6641 | 18.72 | 5.91 | 0.64 |
| 10| 7646 | 29.66 | 9.37 | 0.79 |
| 43| 14281 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10082 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 40.29 | 11.71 | 0.59 |
| 4 | 226 | 858 | 49.52 | 14.31 | 0.69 |
| 5 | 285 | 969 | 64.15 | 18.20 | 0.84 |
| 6 | 339 | 1081 | 68.72 | 19.80 | 0.90 |
| 7 | 395 | 1192 | 84.64 | 23.94 | 1.06 |
| 8 | 449 | 1303 | 92.53 | 26.28 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1821 | 24.00 | 7.62 | 0.48 |
| 2| 1946 | 25.39 | 8.68 | 0.50 |
| 3| 2017 | 26.36 | 9.59 | 0.52 |
| 5| 2491 | 32.07 | 12.54 | 0.61 |
| 10| 3154 | 40.58 | 18.25 | 0.75 |
| 40| 7627 | 96.81 | 53.91 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.84 | 7.37 | 0.42 |
| 2| 826 | 25.59 | 8.82 | 0.46 |
| 3| 900 | 25.07 | 9.31 | 0.46 |
| 5| 1217 | 29.55 | 11.93 | 0.53 |
| 10| 1974 | 39.67 | 18.08 | 0.69 |
| 41| 6544 | 95.51 | 54.30 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 29.17 | 8.91 | 0.48 |
| 2| 835 | 29.22 | 9.61 | 0.49 |
| 3| 872 | 32.09 | 11.03 | 0.53 |
| 5| 1301 | 35.68 | 13.45 | 0.59 |
| 10| 1996 | 47.44 | 20.04 | 0.77 |
| 37| 6042 | 98.58 | 52.39 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 33.87 | 10.16 | 0.53 |
| 2| 859 | 36.64 | 11.62 | 0.57 |
| 3| 959 | 37.95 | 12.63 | 0.59 |
| 5| 1276 | 42.61 | 15.27 | 0.66 |
| 10| 2135 | 55.47 | 22.24 | 0.85 |
| 29| 4929 | 99.86 | 47.27 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5809 | 27.12 | 9.10 | 0.69 |
| 2| 5775 | 28.49 | 9.38 | 0.70 |
| 3| 6162 | 46.00 | 15.50 | 0.90 |
| 4| 6225 | 51.29 | 17.23 | 0.96 |
| 5| 6547 | 66.31 | 22.39 | 1.14 |
| 6| 6569 | 73.89 | 24.89 | 1.22 |
| 7| 6599 | 78.48 | 26.39 | 1.26 |
| 8| 6930 | 91.32 | 30.87 | 1.42 |
| 9| 6923 | 98.87 | 33.30 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 284 | 6004 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.69 | 14.52 | 0.85 |
| 10 | 30 | 1710 | 6856 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2221 | 7160 | 98.42 | 37.71 | 1.53 |

