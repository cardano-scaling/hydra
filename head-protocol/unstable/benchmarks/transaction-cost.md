--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-10 11:26:07.611998034 UTC |
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
| 1| 5834 | 10.17 | 3.22 | 0.51 |
| 2| 6037 | 12.61 | 4.00 | 0.55 |
| 3| 6243 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14279 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10053 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 169 | 747 | 40.25 | 11.70 | 0.59 |
| 4 | 225 | 858 | 48.10 | 13.97 | 0.68 |
| 5 | 283 | 969 | 64.38 | 18.29 | 0.84 |
| 6 | 338 | 1081 | 75.11 | 21.22 | 0.96 |
| 7 | 394 | 1196 | 72.66 | 21.03 | 0.94 |
| 8 | 448 | 1303 | 85.54 | 24.65 | 1.08 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 24.29 | 7.69 | 0.48 |
| 2| 2019 | 26.84 | 9.06 | 0.52 |
| 3| 2053 | 27.35 | 9.87 | 0.53 |
| 5| 2390 | 31.08 | 12.26 | 0.59 |
| 10| 3260 | 43.28 | 19.00 | 0.78 |
| 40| 7579 | 97.06 | 53.92 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 22.84 | 7.39 | 0.42 |
| 2| 771 | 23.51 | 8.21 | 0.43 |
| 3| 834 | 24.13 | 9.06 | 0.45 |
| 5| 1244 | 31.17 | 12.36 | 0.55 |
| 10| 1883 | 37.47 | 17.44 | 0.66 |
| 41| 6571 | 96.71 | 54.62 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 992 | 31.65 | 10.97 | 0.53 |
| 5| 1341 | 35.68 | 13.45 | 0.59 |
| 10| 1879 | 45.97 | 19.60 | 0.75 |
| 35| 6062 | 98.34 | 51.13 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 878 | 36.64 | 11.62 | 0.57 |
| 3| 943 | 37.84 | 12.60 | 0.59 |
| 5| 1263 | 42.49 | 15.24 | 0.66 |
| 10| 1966 | 53.46 | 21.62 | 0.82 |
| 29| 5054 | 99.58 | 47.22 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.05 | 9.07 | 0.69 |
| 2| 5983 | 36.85 | 12.43 | 0.80 |
| 3| 6105 | 45.77 | 15.46 | 0.90 |
| 4| 6265 | 54.94 | 18.50 | 1.00 |
| 5| 6422 | 65.09 | 21.96 | 1.12 |
| 6| 6717 | 76.52 | 25.87 | 1.25 |
| 7| 6695 | 80.72 | 27.20 | 1.29 |
| 8| 6941 | 94.36 | 31.86 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6174 | 40.39 | 14.75 | 0.85 |
| 10 | 30 | 1705 | 6851 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2217 | 7156 | 98.24 | 37.65 | 1.53 |

