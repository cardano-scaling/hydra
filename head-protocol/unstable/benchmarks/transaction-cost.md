--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 11:02:18.072890051 UTC |
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
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.76 | 4.67 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 747 | 42.79 | 12.34 | 0.62 |
| 4 | 226 | 858 | 48.40 | 14.06 | 0.68 |
| 5 | 281 | 969 | 62.98 | 17.92 | 0.83 |
| 6 | 339 | 1081 | 71.51 | 20.39 | 0.92 |
| 7 | 395 | 1192 | 80.52 | 22.91 | 1.02 |
| 8 | 449 | 1303 | 80.60 | 23.32 | 1.03 |
| 9 | 506 | 1418 | 93.36 | 26.82 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 23.92 | 7.60 | 0.48 |
| 2| 1924 | 25.76 | 8.76 | 0.51 |
| 3| 2055 | 27.47 | 9.90 | 0.53 |
| 5| 2446 | 32.04 | 12.53 | 0.61 |
| 10| 3216 | 42.97 | 18.91 | 0.78 |
| 39| 7568 | 97.97 | 53.53 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 22.50 | 7.30 | 0.41 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 849 | 24.07 | 9.03 | 0.45 |
| 5| 1134 | 28.00 | 11.47 | 0.51 |
| 10| 2018 | 42.38 | 18.81 | 0.72 |
| 41| 6687 | 98.24 | 55.03 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 29.13 | 8.90 | 0.48 |
| 2| 849 | 29.94 | 9.83 | 0.50 |
| 3| 869 | 32.08 | 11.03 | 0.53 |
| 5| 1366 | 38.37 | 14.19 | 0.62 |
| 10| 2015 | 47.48 | 20.05 | 0.77 |
| 37| 6145 | 98.70 | 52.50 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 704 | 33.79 | 10.15 | 0.53 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 896 | 37.13 | 12.38 | 0.58 |
| 5| 1312 | 43.27 | 15.47 | 0.67 |
| 10| 2141 | 56.23 | 22.46 | 0.86 |
| 30| 4874 | 99.06 | 47.63 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 26.92 | 9.04 | 0.69 |
| 2| 5892 | 34.87 | 11.66 | 0.78 |
| 3| 6017 | 43.94 | 14.74 | 0.88 |
| 4| 6387 | 56.28 | 18.99 | 1.02 |
| 5| 6262 | 56.64 | 19.00 | 1.02 |
| 6| 6616 | 74.40 | 25.07 | 1.22 |
| 7| 6924 | 85.52 | 28.86 | 1.35 |
| 8| 6815 | 91.50 | 30.73 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 20 | 1138 | 6513 | 59.73 | 22.44 | 1.08 |
| 10 | 30 | 1708 | 6854 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2220 | 7160 | 98.68 | 37.80 | 1.54 |

