--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 12:24:23.885929485 UTC |
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
| 1| 5836 | 10.95 | 3.49 | 0.52 |
| 2| 6037 | 13.08 | 4.16 | 0.55 |
| 3| 6236 | 14.69 | 4.65 | 0.58 |
| 5| 6640 | 18.58 | 5.86 | 0.63 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14279 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10055 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 170 | 747 | 42.57 | 12.25 | 0.62 |
| 4 | 225 | 858 | 49.57 | 14.31 | 0.69 |
| 5 | 283 | 969 | 64.42 | 18.27 | 0.85 |
| 6 | 340 | 1081 | 66.72 | 19.29 | 0.88 |
| 7 | 395 | 1196 | 72.39 | 20.96 | 0.94 |
| 8 | 452 | 1303 | 94.11 | 26.70 | 1.16 |
| 9 | 505 | 1414 | 98.05 | 27.95 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.37 | 7.71 | 0.48 |
| 2| 1940 | 25.92 | 8.80 | 0.51 |
| 3| 2113 | 28.30 | 10.14 | 0.55 |
| 5| 2408 | 31.41 | 12.34 | 0.60 |
| 10| 3258 | 43.27 | 18.98 | 0.78 |
| 41| 7688 | 97.91 | 54.84 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 22.84 | 7.40 | 0.42 |
| 2| 839 | 25.41 | 8.77 | 0.46 |
| 3| 988 | 26.14 | 9.61 | 0.47 |
| 5| 1250 | 30.15 | 12.07 | 0.54 |
| 10| 1964 | 39.57 | 18.05 | 0.69 |
| 41| 6321 | 92.61 | 53.43 | 1.56 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.54 | 8.47 | 0.46 |
| 2| 766 | 28.51 | 9.39 | 0.48 |
| 3| 868 | 32.01 | 11.01 | 0.53 |
| 5| 1264 | 37.58 | 13.95 | 0.61 |
| 10| 2134 | 46.02 | 19.70 | 0.76 |
| 38| 6012 | 98.53 | 53.00 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 33.83 | 10.15 | 0.53 |
| 2| 837 | 36.56 | 11.60 | 0.57 |
| 3| 1077 | 39.34 | 13.05 | 0.61 |
| 5| 1157 | 41.11 | 14.82 | 0.64 |
| 10| 2033 | 53.91 | 21.77 | 0.83 |
| 29| 4895 | 99.07 | 47.01 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 23.01 | 7.58 | 0.64 |
| 2| 5868 | 34.91 | 11.69 | 0.77 |
| 3| 6084 | 45.00 | 15.11 | 0.89 |
| 4| 6235 | 53.95 | 18.14 | 0.99 |
| 5| 6377 | 63.73 | 21.46 | 1.10 |
| 6| 6570 | 73.08 | 24.66 | 1.21 |
| 7| 6839 | 84.01 | 28.37 | 1.33 |
| 8| 6839 | 89.94 | 30.27 | 1.40 |
| 9| 7030 | 98.89 | 33.36 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 286 | 6006 | 28.02 | 9.98 | 0.71 |
| 10 | 20 | 1139 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1709 | 6855 | 79.34 | 30.22 | 1.31 |
| 10 | 39 | 2218 | 7157 | 99.38 | 38.04 | 1.54 |

