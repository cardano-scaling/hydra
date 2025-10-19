--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-19 04:42:13.550990485 UTC |
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
| 1| 5834 | 10.85 | 3.45 | 0.52 |
| 2| 6042 | 12.67 | 4.01 | 0.55 |
| 3| 6238 | 14.60 | 4.62 | 0.58 |
| 5| 6641 | 18.71 | 5.91 | 0.64 |
| 10| 7651 | 29.26 | 9.23 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1273 | 6.41 | 3.60 | 0.28 |
| 10| 2165 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 112 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 169 | 747 | 43.56 | 12.48 | 0.63 |
| 4 | 227 | 858 | 50.99 | 14.68 | 0.71 |
| 5 | 282 | 969 | 56.71 | 16.49 | 0.77 |
| 6 | 340 | 1081 | 70.21 | 20.09 | 0.91 |
| 7 | 395 | 1196 | 86.66 | 24.42 | 1.08 |
| 8 | 451 | 1303 | 96.72 | 27.28 | 1.19 |
| 9 | 506 | 1414 | 90.95 | 26.24 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.29 | 7.69 | 0.48 |
| 2| 1928 | 25.39 | 8.68 | 0.50 |
| 3| 2013 | 26.24 | 9.56 | 0.52 |
| 5| 2422 | 32.02 | 12.53 | 0.61 |
| 10| 3189 | 41.24 | 18.43 | 0.76 |
| 39| 7369 | 94.32 | 52.52 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 604 | 22.57 | 7.30 | 0.41 |
| 2| 833 | 25.57 | 8.80 | 0.46 |
| 3| 895 | 24.99 | 9.29 | 0.46 |
| 5| 1144 | 28.07 | 11.48 | 0.51 |
| 10| 2106 | 42.20 | 18.76 | 0.72 |
| 42| 6723 | 99.92 | 56.18 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.17 | 8.91 | 0.48 |
| 2| 801 | 30.94 | 10.07 | 0.51 |
| 3| 945 | 30.82 | 10.73 | 0.52 |
| 5| 1332 | 35.68 | 13.45 | 0.59 |
| 10| 2078 | 47.93 | 20.20 | 0.77 |
| 37| 6073 | 98.74 | 52.45 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.15 | 9.95 | 0.52 |
| 2| 769 | 35.17 | 11.17 | 0.55 |
| 3| 967 | 37.80 | 12.59 | 0.59 |
| 5| 1429 | 44.62 | 15.89 | 0.69 |
| 10| 2059 | 54.35 | 21.90 | 0.84 |
| 29| 4832 | 96.51 | 46.28 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5822 | 27.08 | 9.08 | 0.69 |
| 2| 5938 | 35.89 | 12.06 | 0.79 |
| 3| 6035 | 41.52 | 13.89 | 0.85 |
| 4| 6210 | 54.27 | 18.24 | 0.99 |
| 5| 6521 | 67.97 | 22.97 | 1.15 |
| 6| 6567 | 71.38 | 24.00 | 1.19 |
| 7| 6813 | 81.72 | 27.60 | 1.31 |
| 8| 7010 | 96.29 | 32.60 | 1.47 |
| 9| 6915 | 98.19 | 33.04 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 29.53 | 10.50 | 0.73 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 30 | 1704 | 6851 | 79.78 | 30.37 | 1.32 |
| 10 | 40 | 2277 | 7194 | 99.66 | 38.24 | 1.55 |

