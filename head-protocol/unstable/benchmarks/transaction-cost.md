--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 14:40:13.424117905 UTC |
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
| 1| 5837 | 10.61 | 3.37 | 0.52 |
| 2| 6038 | 12.23 | 3.86 | 0.54 |
| 3| 6240 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 19.02 | 6.02 | 0.64 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10051 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 39.82 | 11.57 | 0.59 |
| 4 | 225 | 858 | 51.05 | 14.67 | 0.71 |
| 5 | 282 | 969 | 57.72 | 16.66 | 0.78 |
| 6 | 338 | 1081 | 68.22 | 19.61 | 0.89 |
| 7 | 396 | 1192 | 74.71 | 21.60 | 0.96 |
| 8 | 448 | 1303 | 89.29 | 25.45 | 1.11 |
| 9 | 506 | 1414 | 93.51 | 26.86 | 1.16 |
| 10 | 560 | 1529 | 97.58 | 28.36 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 1940 | 25.51 | 8.70 | 0.50 |
| 3| 2107 | 28.30 | 10.14 | 0.54 |
| 5| 2416 | 32.19 | 12.57 | 0.61 |
| 10| 3309 | 43.60 | 19.08 | 0.79 |
| 37| 7099 | 91.85 | 50.47 | 1.57 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.54 | 7.30 | 0.41 |
| 2| 817 | 25.53 | 8.79 | 0.46 |
| 3| 880 | 25.55 | 9.48 | 0.46 |
| 5| 1203 | 29.18 | 11.81 | 0.52 |
| 10| 2038 | 42.22 | 18.78 | 0.72 |
| 39| 6517 | 98.22 | 53.68 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 663 | 29.17 | 8.91 | 0.48 |
| 2| 878 | 29.86 | 9.81 | 0.50 |
| 3| 951 | 30.90 | 10.74 | 0.52 |
| 5| 1215 | 37.13 | 13.80 | 0.60 |
| 10| 2000 | 47.73 | 20.13 | 0.77 |
| 35| 6031 | 97.50 | 50.88 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 33.83 | 10.15 | 0.53 |
| 2| 760 | 35.17 | 11.17 | 0.55 |
| 3| 954 | 37.95 | 12.63 | 0.59 |
| 5| 1243 | 42.65 | 15.28 | 0.66 |
| 10| 1982 | 53.42 | 21.61 | 0.82 |
| 30| 4974 | 99.94 | 47.89 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 27.05 | 9.07 | 0.69 |
| 2| 6014 | 37.05 | 12.49 | 0.80 |
| 3| 5970 | 40.32 | 13.44 | 0.84 |
| 4| 6286 | 53.96 | 18.12 | 0.99 |
| 5| 6266 | 58.40 | 19.52 | 1.04 |
| 6| 6470 | 69.84 | 23.45 | 1.17 |
| 7| 6729 | 83.17 | 28.11 | 1.32 |
| 8| 6855 | 89.89 | 30.29 | 1.40 |
| 10| 7074 | 99.88 | 33.68 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 40 | 2279 | 7195 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2220 | 7159 | 98.49 | 37.73 | 1.53 |

