--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-13 04:50:24.079515483 UTC |
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
| 1| 5837 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 13.01 | 4.14 | 0.55 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 18.52 | 5.84 | 0.63 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.39 | 9.43 | 0.51 |
| 3 | 171 | 747 | 43.53 | 12.46 | 0.62 |
| 4 | 228 | 858 | 48.19 | 13.99 | 0.68 |
| 5 | 283 | 974 | 60.91 | 17.42 | 0.81 |
| 6 | 336 | 1081 | 69.14 | 19.78 | 0.90 |
| 7 | 394 | 1192 | 77.41 | 22.11 | 0.99 |
| 8 | 448 | 1303 | 87.45 | 25.01 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 24.37 | 7.71 | 0.48 |
| 2| 1922 | 25.55 | 8.72 | 0.50 |
| 3| 2070 | 27.39 | 9.88 | 0.53 |
| 5| 2363 | 31.38 | 12.33 | 0.60 |
| 10| 3180 | 42.39 | 18.75 | 0.77 |
| 40| 7618 | 97.61 | 54.11 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 22.84 | 7.38 | 0.41 |
| 2| 697 | 22.55 | 7.93 | 0.42 |
| 3| 959 | 27.08 | 9.89 | 0.48 |
| 5| 1202 | 29.08 | 11.77 | 0.52 |
| 10| 1748 | 34.66 | 16.66 | 0.63 |
| 42| 6813 | 99.96 | 56.22 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.50 | 8.46 | 0.46 |
| 2| 816 | 29.22 | 9.61 | 0.49 |
| 3| 899 | 30.23 | 10.54 | 0.51 |
| 5| 1290 | 37.81 | 14.01 | 0.61 |
| 10| 1915 | 46.84 | 19.85 | 0.76 |
| 34| 5746 | 94.77 | 49.41 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 764 | 35.14 | 11.16 | 0.55 |
| 3| 892 | 37.24 | 12.41 | 0.58 |
| 5| 1263 | 42.72 | 15.30 | 0.66 |
| 10| 2058 | 55.03 | 22.11 | 0.84 |
| 29| 5078 | 99.88 | 47.27 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5802 | 27.13 | 9.09 | 0.69 |
| 2| 5999 | 37.12 | 12.52 | 0.80 |
| 3| 6205 | 46.71 | 15.78 | 0.91 |
| 4| 6252 | 54.97 | 18.51 | 1.00 |
| 5| 6329 | 63.28 | 21.21 | 1.09 |
| 6| 6546 | 73.31 | 24.64 | 1.21 |
| 7| 6711 | 79.49 | 26.69 | 1.28 |
| 8| 6913 | 91.39 | 30.80 | 1.41 |
| 9| 6862 | 99.40 | 33.38 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 56 | 5867 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6005 | 29.79 | 10.58 | 0.73 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2218 | 7157 | 98.68 | 37.80 | 1.53 |

