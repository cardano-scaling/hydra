--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 17:16:33.66280289 UTC |
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
| 2| 6035 | 12.65 | 4.01 | 0.55 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6646 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.12 | 9.18 | 0.79 |
| 43| 14285 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 735 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 171 | 751 | 43.80 | 12.53 | 0.63 |
| 4 | 227 | 858 | 52.78 | 15.16 | 0.72 |
| 5 | 283 | 969 | 64.29 | 18.23 | 0.84 |
| 6 | 339 | 1085 | 71.08 | 20.21 | 0.92 |
| 7 | 396 | 1192 | 78.87 | 22.60 | 1.00 |
| 8 | 449 | 1303 | 89.23 | 25.48 | 1.11 |
| 9 | 505 | 1414 | 99.03 | 28.19 | 1.22 |
| 10 | 560 | 1525 | 98.03 | 28.47 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1811 | 24.00 | 7.62 | 0.48 |
| 2| 2019 | 26.92 | 9.08 | 0.52 |
| 3| 2013 | 26.32 | 9.58 | 0.52 |
| 5| 2415 | 32.15 | 12.56 | 0.61 |
| 10| 3234 | 43.25 | 18.98 | 0.78 |
| 42| 7730 | 98.37 | 55.63 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 653 | 22.54 | 7.31 | 0.41 |
| 2| 766 | 24.08 | 8.40 | 0.44 |
| 3| 874 | 25.55 | 9.48 | 0.46 |
| 5| 1212 | 29.12 | 11.78 | 0.52 |
| 10| 1943 | 37.70 | 17.51 | 0.67 |
| 41| 6447 | 96.62 | 54.56 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 27.54 | 8.47 | 0.46 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 1018 | 31.58 | 10.95 | 0.53 |
| 5| 1234 | 37.03 | 13.78 | 0.60 |
| 10| 2063 | 48.19 | 20.28 | 0.78 |
| 34| 5662 | 93.67 | 49.04 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 880 | 36.56 | 11.60 | 0.57 |
| 3| 967 | 37.84 | 12.60 | 0.59 |
| 5| 1231 | 42.01 | 15.08 | 0.65 |
| 10| 2002 | 53.95 | 21.78 | 0.83 |
| 29| 4715 | 95.99 | 46.09 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5821 | 31.48 | 10.46 | 0.74 |
| 3| 6111 | 45.74 | 15.41 | 0.90 |
| 4| 6229 | 55.58 | 18.74 | 1.01 |
| 5| 6450 | 65.11 | 21.97 | 1.12 |
| 6| 6462 | 65.21 | 21.87 | 1.12 |
| 7| 6994 | 86.22 | 29.28 | 1.37 |
| 8| 7038 | 95.34 | 32.18 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 56 | 5867 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 40 | 2276 | 7193 | 99.66 | 38.24 | 1.55 |

