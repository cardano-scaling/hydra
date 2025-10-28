--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 14:54:58.881202672 UTC |
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
| 1| 5838 | 10.57 | 3.36 | 0.52 |
| 2| 6042 | 12.34 | 3.90 | 0.54 |
| 3| 6236 | 14.97 | 4.75 | 0.58 |
| 5| 6641 | 18.83 | 5.95 | 0.64 |
| 10| 7644 | 29.14 | 9.19 | 0.79 |
| 43| 14285 | 99.11 | 30.98 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.38 | 9.91 | 0.53 |
| 3 | 170 | 751 | 42.73 | 12.29 | 0.62 |
| 4 | 227 | 858 | 48.14 | 14.00 | 0.68 |
| 5 | 283 | 969 | 64.16 | 18.20 | 0.84 |
| 6 | 339 | 1081 | 71.94 | 20.50 | 0.93 |
| 7 | 395 | 1192 | 86.73 | 24.40 | 1.08 |
| 8 | 451 | 1307 | 96.17 | 27.10 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 24.37 | 7.71 | 0.48 |
| 2| 1886 | 24.40 | 8.40 | 0.49 |
| 3| 2013 | 25.87 | 9.47 | 0.52 |
| 5| 2322 | 30.12 | 11.99 | 0.58 |
| 10| 3293 | 44.00 | 19.20 | 0.79 |
| 43| 7868 | 98.17 | 56.25 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.77 | 7.36 | 0.42 |
| 2| 799 | 23.56 | 8.22 | 0.44 |
| 3| 940 | 26.60 | 9.77 | 0.48 |
| 5| 1278 | 30.75 | 12.26 | 0.54 |
| 10| 2086 | 39.80 | 18.10 | 0.69 |
| 46| 6980 | 99.66 | 58.72 | 1.69 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 699 | 27.51 | 8.47 | 0.46 |
| 2| 909 | 29.97 | 9.84 | 0.50 |
| 3| 988 | 33.40 | 11.44 | 0.55 |
| 5| 1131 | 35.56 | 13.34 | 0.58 |
| 10| 2165 | 49.38 | 20.64 | 0.79 |
| 35| 5774 | 99.91 | 51.38 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.83 | 10.16 | 0.53 |
| 2| 837 | 35.85 | 11.38 | 0.56 |
| 3| 987 | 38.51 | 12.80 | 0.60 |
| 5| 1238 | 42.53 | 15.25 | 0.66 |
| 10| 1944 | 52.93 | 21.47 | 0.82 |
| 30| 4939 | 98.97 | 47.61 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5842 | 27.05 | 9.08 | 0.69 |
| 2| 5916 | 34.94 | 11.70 | 0.78 |
| 3| 6089 | 44.56 | 15.01 | 0.89 |
| 4| 6184 | 54.09 | 18.18 | 0.99 |
| 5| 6390 | 62.52 | 21.07 | 1.09 |
| 6| 6501 | 67.44 | 22.66 | 1.14 |
| 7| 6626 | 79.20 | 26.64 | 1.27 |
| 8| 6780 | 91.58 | 30.86 | 1.41 |
| 9| 7015 | 98.32 | 33.07 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 1 | 57 | 5868 | 22.55 | 7.67 | 0.65 |
| 10 | 5 | 285 | 6005 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 568 | 6172 | 40.39 | 14.75 | 0.85 |
| 10 | 20 | 1136 | 6511 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1707 | 6854 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2220 | 7159 | 96.72 | 37.13 | 1.51 |

