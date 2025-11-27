--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 13:44:06.383962972 UTC |
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
| 1| 5836 | 10.26 | 3.25 | 0.51 |
| 2| 6038 | 12.65 | 4.01 | 0.55 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 29.12 | 9.18 | 0.79 |
| 43| 14285 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 43.83 | 12.55 | 0.63 |
| 4 | 228 | 858 | 50.64 | 14.55 | 0.70 |
| 5 | 283 | 969 | 56.48 | 16.40 | 0.77 |
| 6 | 340 | 1081 | 66.25 | 19.10 | 0.87 |
| 7 | 394 | 1192 | 77.82 | 22.30 | 0.99 |
| 8 | 449 | 1303 | 95.53 | 26.94 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1816 | 24.37 | 7.71 | 0.48 |
| 2| 2005 | 26.55 | 9.00 | 0.52 |
| 3| 2124 | 28.31 | 10.14 | 0.55 |
| 5| 2399 | 31.12 | 12.27 | 0.60 |
| 10| 3229 | 43.12 | 18.94 | 0.78 |
| 43| 7904 | 98.28 | 56.29 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.77 | 7.36 | 0.41 |
| 2| 700 | 22.58 | 7.95 | 0.42 |
| 3| 1017 | 28.10 | 10.19 | 0.50 |
| 5| 1168 | 27.97 | 11.46 | 0.51 |
| 10| 2046 | 39.91 | 18.14 | 0.69 |
| 41| 6601 | 96.88 | 54.66 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 708 | 27.47 | 8.46 | 0.46 |
| 2| 766 | 28.51 | 9.39 | 0.48 |
| 3| 910 | 32.76 | 11.24 | 0.54 |
| 5| 1277 | 34.97 | 13.23 | 0.58 |
| 10| 2076 | 48.44 | 20.35 | 0.78 |
| 36| 6057 | 99.76 | 52.12 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 33.79 | 10.15 | 0.53 |
| 2| 837 | 35.92 | 11.40 | 0.56 |
| 3| 951 | 37.95 | 12.63 | 0.59 |
| 5| 1265 | 42.64 | 15.28 | 0.66 |
| 10| 2076 | 54.76 | 22.02 | 0.84 |
| 28| 4787 | 95.21 | 45.23 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5817 | 27.05 | 9.07 | 0.69 |
| 2| 5967 | 37.13 | 12.53 | 0.80 |
| 3| 6060 | 44.75 | 15.04 | 0.89 |
| 4| 6146 | 50.46 | 16.87 | 0.95 |
| 5| 6244 | 58.56 | 19.61 | 1.04 |
| 6| 6548 | 73.40 | 24.64 | 1.21 |
| 7| 6397 | 66.96 | 22.33 | 1.13 |
| 8| 6785 | 88.03 | 29.57 | 1.37 |
| 9| 6926 | 96.28 | 32.41 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6174 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1139 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1707 | 6854 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2273 | 7189 | 99.66 | 38.24 | 1.55 |
| 10 | 38 | 2165 | 7127 | 96.44 | 36.92 | 1.51 |

