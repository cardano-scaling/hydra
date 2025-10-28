--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 21:39:39.754645793 UTC |
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
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.97 | 4.75 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7650 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 39.86 | 11.58 | 0.59 |
| 4 | 226 | 858 | 52.66 | 15.08 | 0.72 |
| 5 | 281 | 969 | 61.05 | 17.49 | 0.81 |
| 6 | 339 | 1085 | 74.99 | 21.19 | 0.96 |
| 7 | 394 | 1192 | 82.37 | 23.35 | 1.04 |
| 8 | 451 | 1303 | 94.48 | 26.70 | 1.16 |
| 9 | 505 | 1414 | 90.78 | 26.15 | 1.13 |
| 10 | 560 | 1525 | 96.30 | 27.86 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.37 | 7.71 | 0.48 |
| 2| 1886 | 24.47 | 8.41 | 0.49 |
| 3| 2182 | 29.01 | 10.35 | 0.56 |
| 5| 2399 | 32.48 | 12.64 | 0.61 |
| 10| 3169 | 40.85 | 18.34 | 0.75 |
| 38| 7427 | 96.67 | 52.52 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 621 | 22.53 | 7.30 | 0.41 |
| 2| 741 | 23.58 | 8.23 | 0.43 |
| 3| 968 | 26.95 | 9.86 | 0.48 |
| 5| 1229 | 29.39 | 11.86 | 0.53 |
| 10| 1958 | 38.50 | 17.74 | 0.68 |
| 42| 6659 | 98.72 | 55.87 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 641 | 29.13 | 8.90 | 0.48 |
| 2| 828 | 31.58 | 10.26 | 0.52 |
| 3| 1054 | 32.29 | 11.17 | 0.54 |
| 5| 1216 | 37.02 | 13.78 | 0.60 |
| 10| 2056 | 47.88 | 20.19 | 0.77 |
| 36| 5886 | 96.91 | 51.29 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.16 | 0.53 |
| 2| 802 | 35.85 | 11.38 | 0.56 |
| 3| 1024 | 38.55 | 12.81 | 0.60 |
| 5| 1397 | 43.91 | 15.67 | 0.68 |
| 10| 2094 | 54.77 | 22.02 | 0.84 |
| 28| 4605 | 94.94 | 45.15 | 1.45 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5839 | 27.05 | 9.08 | 0.69 |
| 2| 5820 | 31.56 | 10.49 | 0.74 |
| 3| 6187 | 46.90 | 15.83 | 0.92 |
| 4| 6115 | 46.95 | 15.69 | 0.91 |
| 5| 6359 | 60.60 | 20.36 | 1.07 |
| 6| 6556 | 73.15 | 24.62 | 1.21 |
| 7| 6723 | 82.87 | 27.95 | 1.32 |
| 8| 6771 | 87.82 | 29.49 | 1.37 |
| 9| 6803 | 94.45 | 31.68 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 5 | 286 | 6005 | 30.42 | 10.80 | 0.74 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1707 | 6854 | 81.37 | 30.91 | 1.33 |
| 10 | 40 | 2277 | 7193 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2215 | 7154 | 98.49 | 37.73 | 1.53 |

