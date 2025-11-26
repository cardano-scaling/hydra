--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-26 04:43:04.82110085 UTC |
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
| 1| 5837 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6242 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 18.58 | 5.86 | 0.63 |
| 10| 7644 | 29.12 | 9.18 | 0.79 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1272 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 112 | 635 | 32.23 | 9.37 | 0.51 |
| 3 | 170 | 751 | 40.25 | 11.70 | 0.59 |
| 4 | 227 | 858 | 53.77 | 15.30 | 0.73 |
| 5 | 282 | 969 | 62.45 | 17.79 | 0.83 |
| 6 | 341 | 1081 | 71.86 | 20.44 | 0.93 |
| 7 | 394 | 1192 | 82.64 | 23.46 | 1.04 |
| 8 | 450 | 1303 | 92.08 | 26.12 | 1.14 |
| 9 | 506 | 1418 | 89.07 | 25.85 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1822 | 24.00 | 7.62 | 0.48 |
| 2| 1999 | 26.84 | 9.06 | 0.52 |
| 3| 2158 | 29.02 | 10.36 | 0.55 |
| 5| 2397 | 31.00 | 12.24 | 0.59 |
| 10| 3120 | 40.60 | 18.25 | 0.75 |
| 42| 7783 | 99.93 | 56.11 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.80 | 7.37 | 0.41 |
| 2| 722 | 22.52 | 7.93 | 0.42 |
| 3| 933 | 26.12 | 9.61 | 0.47 |
| 5| 1183 | 30.12 | 12.08 | 0.53 |
| 10| 2035 | 40.36 | 18.27 | 0.70 |
| 43| 6621 | 95.97 | 55.72 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 704 | 27.51 | 8.47 | 0.46 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 969 | 30.98 | 10.76 | 0.52 |
| 5| 1299 | 35.65 | 13.44 | 0.59 |
| 10| 2074 | 45.54 | 19.56 | 0.75 |
| 35| 5971 | 97.07 | 50.74 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 33.83 | 10.16 | 0.53 |
| 2| 877 | 36.48 | 11.58 | 0.57 |
| 3| 1010 | 38.63 | 12.83 | 0.60 |
| 5| 1217 | 42.27 | 15.16 | 0.65 |
| 10| 2029 | 54.14 | 21.83 | 0.83 |
| 28| 4714 | 96.36 | 45.56 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 27.12 | 9.10 | 0.69 |
| 2| 6012 | 36.97 | 12.45 | 0.80 |
| 3| 6062 | 42.61 | 14.30 | 0.86 |
| 4| 6213 | 53.62 | 18.01 | 0.99 |
| 5| 6329 | 60.23 | 20.25 | 1.06 |
| 6| 6479 | 69.59 | 23.36 | 1.17 |
| 7| 6811 | 84.51 | 28.54 | 1.34 |
| 8| 6735 | 83.62 | 28.12 | 1.32 |
| 9| 6855 | 93.57 | 31.42 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 570 | 6175 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1137 | 6511 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1707 | 6854 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2220 | 7159 | 97.79 | 37.50 | 1.53 |

