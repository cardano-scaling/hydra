--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-30 09:54:58.303248995 UTC |
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
| 1| 5838 | 10.36 | 3.28 | 0.51 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6239 | 14.52 | 4.59 | 0.58 |
| 5| 6643 | 18.84 | 5.95 | 0.64 |
| 10| 7647 | 29.47 | 9.30 | 0.79 |
| 43| 14281 | 99.40 | 31.09 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 744 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1273 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.23 | 9.37 | 0.51 |
| 3 | 170 | 751 | 42.23 | 12.15 | 0.61 |
| 4 | 226 | 858 | 52.42 | 15.00 | 0.72 |
| 5 | 281 | 969 | 64.14 | 18.19 | 0.84 |
| 6 | 340 | 1081 | 73.57 | 20.85 | 0.94 |
| 7 | 395 | 1196 | 79.41 | 22.82 | 1.01 |
| 8 | 449 | 1303 | 90.18 | 25.77 | 1.12 |
| 9 | 505 | 1414 | 98.61 | 28.08 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1813 | 24.29 | 7.69 | 0.48 |
| 2| 1925 | 25.47 | 8.70 | 0.50 |
| 3| 2123 | 28.13 | 10.10 | 0.54 |
| 5| 2328 | 30.45 | 12.07 | 0.59 |
| 10| 3226 | 42.41 | 18.75 | 0.77 |
| 41| 7704 | 98.40 | 54.98 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.50 | 7.30 | 0.41 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 874 | 25.47 | 9.47 | 0.46 |
| 5| 1188 | 28.16 | 11.51 | 0.51 |
| 10| 1913 | 38.89 | 17.85 | 0.68 |
| 41| 6687 | 98.71 | 55.16 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 29.13 | 8.90 | 0.48 |
| 2| 841 | 29.26 | 9.62 | 0.49 |
| 3| 986 | 31.69 | 10.98 | 0.53 |
| 5| 1264 | 35.08 | 13.26 | 0.58 |
| 10| 2064 | 45.39 | 19.53 | 0.75 |
| 37| 5975 | 97.56 | 52.15 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.83 | 10.16 | 0.53 |
| 2| 863 | 36.60 | 11.61 | 0.57 |
| 3| 1003 | 38.51 | 12.80 | 0.60 |
| 5| 1317 | 43.32 | 15.49 | 0.67 |
| 10| 2081 | 54.28 | 21.88 | 0.84 |
| 30| 4995 | 99.84 | 47.85 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.13 | 9.09 | 0.69 |
| 2| 5897 | 32.57 | 10.87 | 0.75 |
| 3| 6014 | 41.21 | 13.79 | 0.85 |
| 4| 6143 | 50.57 | 16.93 | 0.95 |
| 5| 6525 | 66.08 | 22.31 | 1.13 |
| 6| 6506 | 72.17 | 24.22 | 1.19 |
| 7| 6724 | 84.55 | 28.68 | 1.34 |
| 8| 6950 | 95.52 | 32.30 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 568 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1140 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1707 | 6854 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2219 | 7158 | 99.38 | 38.04 | 1.54 |

