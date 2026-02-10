--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-10 13:24:01.51443135 UTC |
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
| 1| 5836 | 10.76 | 3.42 | 0.52 |
| 2| 6041 | 13.01 | 4.14 | 0.55 |
| 3| 6243 | 14.72 | 4.66 | 0.58 |
| 5| 6646 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 29.14 | 9.19 | 0.79 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 112 | 639 | 33.33 | 9.64 | 0.52 |
| 3 | 170 | 747 | 43.77 | 12.52 | 0.63 |
| 4 | 227 | 862 | 51.14 | 14.69 | 0.71 |
| 5 | 284 | 969 | 56.68 | 16.51 | 0.77 |
| 6 | 341 | 1081 | 67.80 | 19.47 | 0.89 |
| 7 | 396 | 1192 | 76.41 | 21.92 | 0.98 |
| 8 | 450 | 1303 | 94.48 | 26.74 | 1.16 |
| 9 | 505 | 1414 | 95.91 | 27.38 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 24.37 | 7.71 | 0.48 |
| 2| 1922 | 25.88 | 8.79 | 0.51 |
| 3| 2129 | 28.46 | 10.18 | 0.55 |
| 5| 2421 | 32.21 | 12.57 | 0.61 |
| 10| 3247 | 44.14 | 19.23 | 0.79 |
| 39| 7642 | 99.54 | 53.96 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 607 | 22.57 | 7.31 | 0.41 |
| 2| 741 | 24.31 | 8.45 | 0.44 |
| 3| 1007 | 27.77 | 10.09 | 0.49 |
| 5| 1239 | 31.09 | 12.34 | 0.54 |
| 10| 2006 | 39.68 | 18.07 | 0.69 |
| 38| 6131 | 92.09 | 51.31 | 1.53 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.54 | 8.47 | 0.46 |
| 2| 740 | 30.23 | 9.85 | 0.50 |
| 3| 1021 | 31.65 | 10.97 | 0.53 |
| 5| 1348 | 35.65 | 13.44 | 0.59 |
| 10| 1833 | 45.49 | 19.44 | 0.74 |
| 36| 5949 | 98.00 | 51.59 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 33.15 | 9.95 | 0.52 |
| 2| 769 | 35.17 | 11.17 | 0.55 |
| 3| 953 | 38.21 | 12.71 | 0.59 |
| 5| 1344 | 43.28 | 15.47 | 0.67 |
| 10| 2205 | 56.35 | 22.49 | 0.86 |
| 30| 4843 | 98.69 | 47.52 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5826 | 27.05 | 9.07 | 0.69 |
| 2| 5981 | 35.99 | 12.09 | 0.79 |
| 3| 6148 | 45.77 | 15.42 | 0.90 |
| 4| 6343 | 56.15 | 18.95 | 1.02 |
| 5| 6554 | 66.78 | 22.60 | 1.14 |
| 6| 6493 | 69.58 | 23.34 | 1.17 |
| 7| 6687 | 79.50 | 26.76 | 1.28 |
| 8| 6876 | 92.74 | 31.33 | 1.43 |
| 9| 6846 | 94.06 | 31.56 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.05 | 6.02 | 0.60 |
| 10 | 5 | 283 | 6003 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 568 | 6172 | 38.81 | 14.21 | 0.84 |
| 10 | 20 | 1140 | 6515 | 58.66 | 22.07 | 1.07 |
| 10 | 30 | 1707 | 6853 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2275 | 7192 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2220 | 7160 | 98.93 | 37.88 | 1.54 |

