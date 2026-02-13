--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-13 11:45:40.747566118 UTC |
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
| 1| 5836 | 10.48 | 3.33 | 0.52 |
| 2| 6038 | 12.61 | 4.00 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 19.19 | 6.08 | 0.64 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 170 | 747 | 39.89 | 11.59 | 0.59 |
| 4 | 228 | 858 | 48.35 | 14.05 | 0.68 |
| 5 | 283 | 969 | 57.87 | 16.73 | 0.78 |
| 6 | 342 | 1085 | 64.55 | 18.69 | 0.85 |
| 7 | 395 | 1192 | 87.36 | 24.64 | 1.09 |
| 8 | 450 | 1303 | 80.76 | 23.46 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 24.37 | 7.71 | 0.48 |
| 2| 1922 | 25.47 | 8.69 | 0.50 |
| 3| 2127 | 27.94 | 10.05 | 0.54 |
| 5| 2372 | 31.49 | 12.36 | 0.60 |
| 10| 3112 | 39.51 | 17.95 | 0.74 |
| 41| 7646 | 97.61 | 54.75 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.81 | 7.37 | 0.42 |
| 2| 774 | 23.59 | 8.23 | 0.43 |
| 3| 958 | 27.00 | 9.87 | 0.48 |
| 5| 1253 | 30.62 | 12.24 | 0.54 |
| 10| 1933 | 37.59 | 17.48 | 0.67 |
| 41| 6695 | 97.89 | 54.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 29.17 | 8.91 | 0.48 |
| 2| 819 | 29.26 | 9.62 | 0.49 |
| 3| 1075 | 31.98 | 11.07 | 0.54 |
| 5| 1413 | 36.24 | 13.62 | 0.60 |
| 10| 2114 | 48.79 | 20.45 | 0.78 |
| 36| 5844 | 95.85 | 50.97 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 33.83 | 10.15 | 0.53 |
| 2| 809 | 35.85 | 11.38 | 0.56 |
| 3| 1003 | 38.63 | 12.83 | 0.60 |
| 5| 1251 | 42.72 | 15.30 | 0.66 |
| 10| 2025 | 54.20 | 21.84 | 0.83 |
| 29| 4962 | 99.48 | 47.15 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 27.08 | 9.08 | 0.69 |
| 2| 5935 | 35.92 | 12.05 | 0.79 |
| 3| 6093 | 44.57 | 15.00 | 0.89 |
| 4| 6284 | 55.16 | 18.57 | 1.01 |
| 5| 6395 | 60.15 | 20.26 | 1.06 |
| 6| 6583 | 74.18 | 24.98 | 1.22 |
| 7| 6826 | 82.22 | 27.78 | 1.32 |
| 8| 7004 | 96.35 | 32.60 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.21 | 10.04 | 0.71 |
| 10 | 10 | 570 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 30 | 1706 | 6853 | 79.15 | 30.16 | 1.31 |
| 10 | 38 | 2163 | 7126 | 96.00 | 36.77 | 1.50 |

