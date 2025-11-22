--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-22 04:48:16.160152547 UTC |
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
| 1| 5836 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6645 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 29.18 | 9.20 | 0.79 |
| 43| 14279 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 171 | 747 | 40.09 | 11.66 | 0.59 |
| 4 | 226 | 858 | 48.26 | 14.03 | 0.68 |
| 5 | 282 | 969 | 59.33 | 17.05 | 0.80 |
| 6 | 341 | 1081 | 65.80 | 18.99 | 0.87 |
| 7 | 393 | 1192 | 87.37 | 24.68 | 1.09 |
| 8 | 448 | 1303 | 96.42 | 27.20 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1820 | 24.37 | 7.71 | 0.48 |
| 2| 2009 | 26.47 | 8.98 | 0.52 |
| 3| 2018 | 25.87 | 9.47 | 0.52 |
| 5| 2315 | 30.04 | 11.97 | 0.58 |
| 10| 3333 | 45.07 | 19.49 | 0.80 |
| 41| 7639 | 98.11 | 54.86 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.81 | 7.37 | 0.42 |
| 2| 793 | 25.56 | 8.79 | 0.46 |
| 3| 932 | 26.87 | 9.84 | 0.48 |
| 5| 1140 | 28.11 | 11.49 | 0.51 |
| 10| 2011 | 39.61 | 18.06 | 0.69 |
| 41| 6663 | 98.46 | 55.09 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 29.13 | 8.90 | 0.48 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 1028 | 34.11 | 11.65 | 0.56 |
| 5| 1281 | 37.78 | 14.00 | 0.61 |
| 10| 2106 | 45.43 | 19.54 | 0.75 |
| 38| 6037 | 99.28 | 53.20 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.87 | 10.16 | 0.53 |
| 2| 885 | 36.60 | 11.61 | 0.57 |
| 3| 996 | 38.51 | 12.80 | 0.60 |
| 5| 1267 | 42.72 | 15.30 | 0.66 |
| 10| 2071 | 54.85 | 22.04 | 0.84 |
| 29| 4936 | 99.30 | 47.07 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5793 | 27.00 | 9.06 | 0.69 |
| 2| 5926 | 36.00 | 12.10 | 0.79 |
| 3| 6059 | 44.80 | 15.05 | 0.89 |
| 4| 6363 | 57.47 | 19.42 | 1.03 |
| 5| 6447 | 64.26 | 21.61 | 1.11 |
| 6| 6593 | 74.39 | 25.06 | 1.22 |
| 7| 6693 | 83.79 | 28.28 | 1.33 |
| 8| 6669 | 83.87 | 28.07 | 1.32 |
| 9| 6864 | 94.68 | 31.93 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 569 | 6173 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1137 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1705 | 6851 | 80.04 | 30.46 | 1.32 |
| 10 | 36 | 2051 | 7059 | 92.34 | 35.31 | 1.46 |

