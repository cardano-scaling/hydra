--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-06 18:20:39.177598198 UTC |
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
| 1| 5836 | 10.66 | 3.39 | 0.52 |
| 2| 6037 | 12.61 | 4.00 | 0.55 |
| 3| 6242 | 14.69 | 4.65 | 0.58 |
| 5| 6641 | 18.72 | 5.91 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14279 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 640 | 34.31 | 9.88 | 0.53 |
| 3 | 169 | 751 | 43.75 | 12.55 | 0.63 |
| 4 | 228 | 858 | 47.83 | 13.88 | 0.67 |
| 5 | 282 | 969 | 61.53 | 17.61 | 0.82 |
| 6 | 339 | 1081 | 68.70 | 19.64 | 0.89 |
| 7 | 394 | 1192 | 82.54 | 23.43 | 1.04 |
| 8 | 451 | 1307 | 97.48 | 27.52 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1808 | 24.37 | 7.71 | 0.48 |
| 2| 1993 | 27.00 | 9.10 | 0.52 |
| 3| 2054 | 27.23 | 9.84 | 0.53 |
| 5| 2426 | 32.24 | 12.58 | 0.61 |
| 10| 3235 | 42.82 | 18.87 | 0.77 |
| 39| 7528 | 97.04 | 53.29 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 22.50 | 7.30 | 0.41 |
| 2| 761 | 23.62 | 8.24 | 0.43 |
| 3| 963 | 26.92 | 9.85 | 0.48 |
| 5| 1264 | 30.13 | 12.07 | 0.54 |
| 10| 1925 | 37.50 | 17.46 | 0.66 |
| 42| 6718 | 97.95 | 55.64 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 27.51 | 8.47 | 0.46 |
| 2| 839 | 29.22 | 9.61 | 0.49 |
| 3| 906 | 30.19 | 10.53 | 0.51 |
| 5| 1257 | 35.08 | 13.26 | 0.58 |
| 10| 1951 | 46.75 | 19.85 | 0.76 |
| 36| 6017 | 97.76 | 51.56 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.16 | 0.53 |
| 2| 854 | 36.60 | 11.61 | 0.57 |
| 3| 942 | 37.91 | 12.62 | 0.59 |
| 5| 1228 | 42.01 | 15.08 | 0.65 |
| 10| 2142 | 55.25 | 22.18 | 0.85 |
| 28| 4857 | 96.12 | 45.54 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.08 | 9.08 | 0.69 |
| 2| 5871 | 34.99 | 11.71 | 0.78 |
| 3| 6157 | 45.82 | 15.46 | 0.90 |
| 4| 6209 | 53.42 | 17.95 | 0.98 |
| 5| 6320 | 60.46 | 20.34 | 1.06 |
| 6| 6482 | 71.94 | 24.14 | 1.19 |
| 7| 6620 | 79.04 | 26.51 | 1.27 |
| 8| 6926 | 94.52 | 31.88 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 285 | 6005 | 29.53 | 10.50 | 0.73 |
| 10 | 30 | 1708 | 6855 | 81.11 | 30.83 | 1.33 |
| 10 | 39 | 2218 | 7157 | 98.49 | 37.73 | 1.53 |

