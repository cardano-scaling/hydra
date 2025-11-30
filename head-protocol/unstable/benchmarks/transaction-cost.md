--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-30 04:57:23.786835674 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 18.60 | 5.87 | 0.64 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 170 | 747 | 42.73 | 12.29 | 0.62 |
| 4 | 227 | 858 | 51.34 | 14.79 | 0.71 |
| 5 | 281 | 969 | 57.95 | 16.75 | 0.78 |
| 6 | 340 | 1085 | 75.74 | 21.41 | 0.96 |
| 7 | 395 | 1192 | 80.37 | 22.87 | 1.02 |
| 8 | 451 | 1303 | 84.26 | 24.19 | 1.06 |
| 9 | 504 | 1414 | 96.32 | 27.59 | 1.19 |
| 10 | 561 | 1525 | 96.26 | 27.91 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1819 | 24.00 | 7.62 | 0.48 |
| 2| 1934 | 25.47 | 8.70 | 0.50 |
| 3| 2079 | 26.94 | 9.77 | 0.53 |
| 5| 2315 | 30.45 | 12.07 | 0.59 |
| 10| 3134 | 40.56 | 18.24 | 0.75 |
| 40| 7664 | 98.06 | 54.23 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.77 | 7.36 | 0.42 |
| 2| 768 | 24.00 | 8.40 | 0.44 |
| 3| 899 | 24.99 | 9.29 | 0.46 |
| 5| 1266 | 30.06 | 12.06 | 0.54 |
| 10| 1973 | 39.46 | 18.02 | 0.69 |
| 40| 6635 | 99.86 | 54.87 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.47 | 8.46 | 0.46 |
| 2| 775 | 30.90 | 10.06 | 0.51 |
| 3| 924 | 32.68 | 11.22 | 0.54 |
| 5| 1237 | 37.14 | 13.81 | 0.60 |
| 10| 2080 | 45.01 | 19.39 | 0.75 |
| 36| 5976 | 97.40 | 51.46 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 33.83 | 10.16 | 0.53 |
| 2| 856 | 36.52 | 11.59 | 0.57 |
| 3| 980 | 38.55 | 12.81 | 0.60 |
| 5| 1254 | 42.68 | 15.29 | 0.66 |
| 10| 1999 | 53.95 | 21.78 | 0.83 |
| 30| 4997 | 99.13 | 47.67 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 23.01 | 7.58 | 0.64 |
| 2| 5846 | 31.48 | 10.46 | 0.74 |
| 3| 6093 | 45.07 | 15.15 | 0.89 |
| 4| 6320 | 56.41 | 19.06 | 1.02 |
| 5| 6362 | 62.88 | 21.17 | 1.09 |
| 6| 6749 | 75.89 | 25.74 | 1.25 |
| 7| 6617 | 75.69 | 25.45 | 1.24 |
| 8| 6788 | 90.41 | 30.42 | 1.40 |
| 9| 6885 | 97.36 | 32.73 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1706 | 6852 | 81.37 | 30.91 | 1.33 |
| 10 | 38 | 2165 | 7127 | 95.56 | 36.62 | 1.50 |

