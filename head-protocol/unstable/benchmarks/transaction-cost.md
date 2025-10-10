--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-10 10:13:36.223789322 UTC |
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
| 1| 5836 | 10.69 | 3.40 | 0.52 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6643 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 29.12 | 9.18 | 0.79 |
| 43| 14285 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10080 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 747 | 41.13 | 11.88 | 0.60 |
| 4 | 227 | 858 | 53.78 | 15.30 | 0.73 |
| 5 | 283 | 969 | 60.55 | 17.31 | 0.81 |
| 6 | 339 | 1081 | 75.28 | 21.29 | 0.96 |
| 7 | 394 | 1192 | 82.50 | 23.42 | 1.04 |
| 8 | 451 | 1303 | 98.84 | 27.74 | 1.21 |
| 9 | 506 | 1418 | 99.94 | 28.41 | 1.22 |
| 10 | 562 | 1525 | 96.44 | 27.96 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.00 | 7.62 | 0.48 |
| 2| 1941 | 25.47 | 8.70 | 0.50 |
| 3| 2055 | 26.95 | 9.77 | 0.53 |
| 5| 2428 | 32.33 | 12.60 | 0.61 |
| 10| 3127 | 41.03 | 18.36 | 0.75 |
| 39| 7404 | 95.33 | 52.77 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 834 | 25.20 | 8.73 | 0.45 |
| 3| 853 | 24.11 | 9.04 | 0.45 |
| 5| 1208 | 29.14 | 11.78 | 0.52 |
| 10| 1991 | 38.62 | 17.77 | 0.68 |
| 40| 6521 | 98.75 | 54.49 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 795 | 30.94 | 10.07 | 0.51 |
| 3| 902 | 30.26 | 10.55 | 0.51 |
| 5| 1248 | 36.99 | 13.77 | 0.60 |
| 10| 2066 | 48.79 | 20.46 | 0.78 |
| 35| 5919 | 97.52 | 50.87 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 33.87 | 10.16 | 0.53 |
| 2| 832 | 35.85 | 11.38 | 0.56 |
| 3| 980 | 38.62 | 12.83 | 0.60 |
| 5| 1309 | 43.35 | 15.49 | 0.67 |
| 10| 2125 | 55.47 | 22.24 | 0.85 |
| 29| 5048 | 99.67 | 47.24 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5787 | 27.09 | 9.08 | 0.69 |
| 2| 5963 | 35.91 | 12.06 | 0.79 |
| 3| 6105 | 45.90 | 15.45 | 0.90 |
| 4| 6254 | 54.34 | 18.35 | 1.00 |
| 5| 6243 | 55.83 | 18.63 | 1.01 |
| 6| 6486 | 69.80 | 23.46 | 1.17 |
| 7| 6727 | 79.60 | 26.81 | 1.28 |
| 8| 6781 | 85.23 | 28.63 | 1.34 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 569 | 6174 | 40.83 | 14.90 | 0.86 |
| 10 | 20 | 1137 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1710 | 6856 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2220 | 7160 | 99.38 | 38.04 | 1.54 |

