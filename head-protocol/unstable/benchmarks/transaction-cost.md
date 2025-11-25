--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-25 11:55:57.929346798 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6641 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10041 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 169 | 751 | 42.50 | 12.22 | 0.61 |
| 4 | 228 | 858 | 48.39 | 14.06 | 0.68 |
| 5 | 283 | 969 | 61.50 | 17.57 | 0.82 |
| 6 | 339 | 1081 | 66.51 | 19.20 | 0.87 |
| 7 | 395 | 1192 | 78.54 | 22.48 | 1.00 |
| 8 | 451 | 1303 | 90.52 | 25.85 | 1.13 |
| 9 | 505 | 1414 | 93.03 | 26.63 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1803 | 24.37 | 7.71 | 0.48 |
| 2| 1989 | 26.92 | 9.08 | 0.52 |
| 3| 2078 | 26.87 | 9.75 | 0.53 |
| 5| 2323 | 30.16 | 12.00 | 0.58 |
| 10| 3209 | 43.23 | 18.97 | 0.78 |
| 41| 7648 | 98.70 | 55.04 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.80 | 7.38 | 0.42 |
| 2| 793 | 23.59 | 8.23 | 0.44 |
| 3| 870 | 25.09 | 9.31 | 0.46 |
| 5| 1179 | 29.18 | 11.81 | 0.52 |
| 10| 1923 | 36.75 | 17.25 | 0.66 |
| 40| 6581 | 98.79 | 54.49 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 27.47 | 8.46 | 0.46 |
| 2| 822 | 29.22 | 9.61 | 0.49 |
| 3| 864 | 32.01 | 11.01 | 0.53 |
| 5| 1339 | 38.56 | 14.24 | 0.62 |
| 10| 2122 | 45.76 | 19.62 | 0.75 |
| 34| 5688 | 99.87 | 50.77 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 702 | 33.83 | 10.16 | 0.53 |
| 2| 870 | 36.60 | 11.61 | 0.57 |
| 3| 1008 | 38.59 | 12.82 | 0.60 |
| 5| 1213 | 42.15 | 15.13 | 0.65 |
| 10| 2186 | 56.14 | 22.44 | 0.86 |
| 29| 4978 | 98.98 | 47.01 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5814 | 27.13 | 9.10 | 0.69 |
| 2| 5845 | 31.44 | 10.45 | 0.74 |
| 3| 6020 | 41.28 | 13.81 | 0.85 |
| 4| 6257 | 54.15 | 18.19 | 0.99 |
| 5| 6125 | 51.51 | 17.10 | 0.96 |
| 6| 6298 | 63.81 | 21.36 | 1.10 |
| 7| 6623 | 79.75 | 26.86 | 1.28 |
| 8| 6833 | 89.68 | 30.22 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1140 | 6515 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1709 | 6856 | 80.85 | 30.74 | 1.33 |
| 10 | 39 | 2223 | 7162 | 98.93 | 37.88 | 1.54 |

