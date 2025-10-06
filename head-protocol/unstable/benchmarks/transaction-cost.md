--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-06 04:39:36.955180052 UTC |
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
| 1| 5836 | 10.38 | 3.29 | 0.51 |
| 2| 6035 | 12.42 | 3.93 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.80 | 9.07 | 0.78 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10048 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 170 | 747 | 42.51 | 12.22 | 0.61 |
| 4 | 228 | 858 | 50.98 | 14.68 | 0.71 |
| 5 | 282 | 969 | 59.40 | 17.09 | 0.80 |
| 6 | 339 | 1081 | 68.19 | 19.60 | 0.89 |
| 7 | 395 | 1196 | 87.39 | 24.68 | 1.09 |
| 8 | 450 | 1307 | 97.68 | 27.35 | 1.19 |
| 10 | 561 | 1529 | 99.34 | 28.71 | 1.23 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1816 | 24.37 | 7.71 | 0.48 |
| 2| 1970 | 26.47 | 8.98 | 0.52 |
| 3| 2081 | 26.98 | 9.78 | 0.53 |
| 5| 2323 | 30.05 | 11.97 | 0.58 |
| 10| 3228 | 43.09 | 18.96 | 0.78 |
| 39| 7530 | 97.54 | 53.41 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 22.54 | 7.31 | 0.41 |
| 2| 810 | 25.40 | 8.76 | 0.45 |
| 3| 1005 | 28.04 | 10.16 | 0.49 |
| 5| 1207 | 29.81 | 11.99 | 0.53 |
| 10| 1899 | 38.60 | 17.78 | 0.67 |
| 39| 6339 | 94.99 | 52.76 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 664 | 29.13 | 8.90 | 0.48 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 944 | 30.87 | 10.74 | 0.52 |
| 5| 1224 | 36.94 | 13.75 | 0.60 |
| 10| 2002 | 44.98 | 19.39 | 0.74 |
| 36| 5999 | 97.13 | 51.35 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 699 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 956 | 37.95 | 12.63 | 0.59 |
| 5| 1266 | 42.45 | 15.23 | 0.66 |
| 10| 2000 | 53.90 | 21.77 | 0.83 |
| 28| 4875 | 97.60 | 45.98 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.12 | 9.10 | 0.69 |
| 2| 5890 | 32.52 | 10.87 | 0.75 |
| 3| 6131 | 45.71 | 15.44 | 0.90 |
| 4| 6254 | 54.97 | 18.54 | 1.00 |
| 5| 6437 | 61.65 | 20.75 | 1.08 |
| 6| 6565 | 74.53 | 25.12 | 1.22 |
| 7| 6874 | 85.61 | 29.04 | 1.35 |
| 8| 6882 | 93.46 | 31.47 | 1.44 |
| 9| 6883 | 94.94 | 31.98 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6005 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 569 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1137 | 6512 | 58.66 | 22.07 | 1.07 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2218 | 7157 | 98.05 | 37.58 | 1.53 |

