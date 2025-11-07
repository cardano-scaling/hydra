--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-07 09:47:07.698463421 UTC |
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
| 1| 5834 | 10.28 | 3.25 | 0.51 |
| 2| 6042 | 12.32 | 3.89 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 19.08 | 6.04 | 0.64 |
| 10| 7644 | 28.88 | 9.10 | 0.79 |
| 43| 14281 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.32 | 9.66 | 0.52 |
| 3 | 170 | 747 | 40.00 | 11.63 | 0.59 |
| 4 | 227 | 858 | 52.27 | 14.96 | 0.72 |
| 5 | 282 | 969 | 64.40 | 18.23 | 0.84 |
| 6 | 340 | 1081 | 73.56 | 20.85 | 0.94 |
| 7 | 393 | 1192 | 72.86 | 21.16 | 0.94 |
| 8 | 451 | 1303 | 81.51 | 23.74 | 1.04 |
| 9 | 505 | 1414 | 95.99 | 27.51 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 24.37 | 7.71 | 0.48 |
| 2| 1983 | 26.55 | 9.00 | 0.52 |
| 3| 2114 | 27.94 | 10.05 | 0.54 |
| 5| 2343 | 30.26 | 12.02 | 0.58 |
| 10| 3297 | 44.30 | 19.27 | 0.79 |
| 41| 7681 | 99.73 | 55.31 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.84 | 7.39 | 0.42 |
| 2| 748 | 24.27 | 8.46 | 0.44 |
| 3| 951 | 26.93 | 9.85 | 0.48 |
| 5| 1183 | 28.04 | 11.48 | 0.51 |
| 10| 2086 | 40.56 | 18.32 | 0.70 |
| 40| 6473 | 95.59 | 53.66 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 27.50 | 8.46 | 0.46 |
| 2| 774 | 28.47 | 9.38 | 0.48 |
| 3| 973 | 33.36 | 11.42 | 0.54 |
| 5| 1256 | 34.97 | 13.23 | 0.58 |
| 10| 1891 | 46.13 | 19.64 | 0.75 |
| 34| 5723 | 99.24 | 50.59 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 691 | 33.87 | 10.16 | 0.53 |
| 2| 826 | 35.85 | 11.38 | 0.56 |
| 3| 1087 | 39.14 | 13.00 | 0.61 |
| 5| 1312 | 43.24 | 15.47 | 0.67 |
| 10| 2189 | 56.98 | 22.69 | 0.87 |
| 29| 4858 | 97.79 | 46.62 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5826 | 26.96 | 9.06 | 0.69 |
| 2| 5949 | 35.91 | 12.09 | 0.79 |
| 3| 5968 | 40.40 | 13.47 | 0.84 |
| 4| 6223 | 54.86 | 18.50 | 1.00 |
| 5| 6492 | 66.42 | 22.40 | 1.13 |
| 6| 6402 | 68.49 | 22.95 | 1.15 |
| 7| 6802 | 83.95 | 28.44 | 1.33 |
| 8| 6889 | 94.21 | 31.88 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 569 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1137 | 6511 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1709 | 6855 | 78.71 | 30.00 | 1.30 |
| 10 | 39 | 2223 | 7162 | 98.86 | 37.86 | 1.54 |

