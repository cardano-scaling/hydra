--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-08 19:00:51.480519748 UTC |
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
| 1| 5834 | 10.26 | 3.25 | 0.51 |
| 2| 6037 | 12.44 | 3.94 | 0.54 |
| 3| 6238 | 14.38 | 4.54 | 0.57 |
| 5| 6641 | 19.00 | 6.01 | 0.64 |
| 10| 7650 | 29.12 | 9.18 | 0.79 |
| 43| 14279 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 34.31 | 9.88 | 0.53 |
| 3 | 171 | 747 | 42.45 | 12.22 | 0.61 |
| 4 | 226 | 858 | 49.70 | 14.40 | 0.69 |
| 5 | 283 | 969 | 61.22 | 17.50 | 0.81 |
| 6 | 339 | 1081 | 69.92 | 20.01 | 0.91 |
| 7 | 394 | 1192 | 75.86 | 21.79 | 0.97 |
| 8 | 450 | 1303 | 92.16 | 26.19 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.00 | 7.62 | 0.48 |
| 2| 1882 | 24.48 | 8.42 | 0.49 |
| 3| 2013 | 25.95 | 9.49 | 0.52 |
| 5| 2426 | 31.95 | 12.51 | 0.61 |
| 10| 3039 | 38.78 | 17.74 | 0.73 |
| 40| 7553 | 97.94 | 54.17 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.54 | 7.30 | 0.41 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 978 | 27.04 | 9.88 | 0.48 |
| 5| 1091 | 27.11 | 11.21 | 0.50 |
| 10| 1867 | 36.56 | 17.19 | 0.65 |
| 41| 6628 | 97.48 | 54.86 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 27.50 | 8.46 | 0.46 |
| 2| 766 | 28.51 | 9.39 | 0.48 |
| 3| 981 | 33.39 | 11.43 | 0.55 |
| 5| 1324 | 38.56 | 14.24 | 0.62 |
| 10| 2127 | 48.64 | 20.42 | 0.78 |
| 36| 5678 | 94.08 | 50.42 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 672 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 896 | 37.13 | 12.38 | 0.58 |
| 5| 1224 | 42.01 | 15.08 | 0.65 |
| 10| 2105 | 54.76 | 22.04 | 0.84 |
| 30| 4939 | 99.97 | 47.88 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.97 | 7.57 | 0.64 |
| 2| 6012 | 37.09 | 12.51 | 0.80 |
| 3| 6126 | 46.10 | 15.54 | 0.90 |
| 4| 6258 | 52.56 | 17.72 | 0.98 |
| 5| 6362 | 60.38 | 20.29 | 1.06 |
| 6| 6388 | 64.92 | 21.74 | 1.11 |
| 7| 6707 | 79.71 | 26.79 | 1.28 |
| 8| 7054 | 95.74 | 32.41 | 1.47 |
| 9| 6939 | 99.26 | 33.39 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.82 | 6.63 | 0.61 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6513 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1707 | 6853 | 80.92 | 30.76 | 1.33 |
| 10 | 40 | 2277 | 7193 | 99.40 | 38.15 | 1.54 |

