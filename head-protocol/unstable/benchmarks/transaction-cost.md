--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-23 10:17:16.088097955 UTC |
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
| 2| 6038 | 12.67 | 4.01 | 0.55 |
| 3| 6239 | 14.97 | 4.75 | 0.58 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.00 | 9.14 | 0.79 |
| 43| 14286 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 32.34 | 9.42 | 0.51 |
| 3 | 170 | 747 | 42.54 | 12.22 | 0.61 |
| 4 | 226 | 858 | 54.05 | 15.41 | 0.74 |
| 5 | 282 | 969 | 56.23 | 16.33 | 0.76 |
| 6 | 338 | 1081 | 73.86 | 20.96 | 0.95 |
| 7 | 394 | 1192 | 80.49 | 22.90 | 1.02 |
| 8 | 449 | 1303 | 80.26 | 23.24 | 1.02 |
| 9 | 507 | 1414 | 96.45 | 27.68 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.00 | 7.62 | 0.48 |
| 2| 1987 | 26.62 | 9.02 | 0.52 |
| 3| 2095 | 28.47 | 10.18 | 0.55 |
| 5| 2422 | 32.36 | 12.61 | 0.61 |
| 10| 3161 | 40.49 | 18.23 | 0.75 |
| 43| 7830 | 99.42 | 56.60 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.54 | 7.30 | 0.41 |
| 2| 762 | 23.58 | 8.24 | 0.43 |
| 3| 969 | 28.32 | 10.24 | 0.50 |
| 5| 1179 | 29.06 | 11.76 | 0.52 |
| 10| 1899 | 38.68 | 17.79 | 0.67 |
| 41| 6541 | 94.39 | 53.96 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 697 | 27.54 | 8.47 | 0.46 |
| 2| 876 | 29.97 | 9.84 | 0.50 |
| 3| 903 | 30.26 | 10.55 | 0.51 |
| 5| 1343 | 36.39 | 13.66 | 0.60 |
| 10| 2109 | 45.73 | 19.61 | 0.75 |
| 38| 6078 | 99.25 | 53.23 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 33.87 | 10.16 | 0.53 |
| 2| 760 | 35.21 | 11.18 | 0.55 |
| 3| 987 | 38.55 | 12.81 | 0.60 |
| 5| 1411 | 44.82 | 15.94 | 0.69 |
| 10| 1928 | 52.56 | 21.36 | 0.81 |
| 29| 4907 | 99.24 | 47.06 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5791 | 27.00 | 9.07 | 0.69 |
| 2| 5938 | 36.07 | 12.12 | 0.79 |
| 3| 6037 | 41.24 | 13.80 | 0.85 |
| 4| 6340 | 56.28 | 18.98 | 1.02 |
| 5| 6477 | 63.60 | 21.47 | 1.10 |
| 6| 6735 | 76.37 | 25.88 | 1.25 |
| 7| 6677 | 79.65 | 26.78 | 1.28 |
| 8| 6809 | 92.44 | 31.09 | 1.42 |
| 9| 6831 | 93.96 | 31.62 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6174 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1141 | 6515 | 59.54 | 22.38 | 1.08 |
| 10 | 40 | 2276 | 7192 | 99.66 | 38.24 | 1.55 |
| 10 | 36 | 2049 | 7058 | 91.46 | 35.01 | 1.45 |

