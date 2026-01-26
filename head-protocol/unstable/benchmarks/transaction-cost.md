--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-26 16:54:50.41704386 UTC |
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
| 1| 5836 | 10.61 | 3.37 | 0.52 |
| 2| 6038 | 12.42 | 3.93 | 0.54 |
| 3| 6238 | 15.16 | 4.82 | 0.58 |
| 5| 6638 | 18.58 | 5.86 | 0.63 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.20 | 9.84 | 0.53 |
| 3 | 171 | 747 | 39.81 | 11.57 | 0.59 |
| 4 | 226 | 858 | 48.29 | 14.06 | 0.68 |
| 5 | 281 | 969 | 57.79 | 16.68 | 0.78 |
| 6 | 338 | 1081 | 65.74 | 18.93 | 0.87 |
| 7 | 396 | 1192 | 78.46 | 22.46 | 1.00 |
| 8 | 450 | 1303 | 84.44 | 24.24 | 1.06 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1749 | 22.92 | 7.32 | 0.47 |
| 2| 1972 | 26.55 | 9.00 | 0.52 |
| 3| 2084 | 27.31 | 9.86 | 0.53 |
| 5| 2397 | 31.19 | 12.29 | 0.60 |
| 10| 3107 | 39.51 | 17.95 | 0.74 |
| 41| 7716 | 97.40 | 54.70 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 22.54 | 7.30 | 0.41 |
| 2| 722 | 22.60 | 7.95 | 0.42 |
| 3| 976 | 26.06 | 9.59 | 0.47 |
| 5| 1231 | 29.12 | 11.78 | 0.52 |
| 10| 1888 | 36.43 | 17.15 | 0.65 |
| 41| 6533 | 96.90 | 54.68 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 29.17 | 8.91 | 0.48 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 1002 | 33.40 | 11.43 | 0.55 |
| 5| 1212 | 34.29 | 13.02 | 0.57 |
| 10| 1917 | 46.72 | 19.83 | 0.76 |
| 38| 6162 | 98.90 | 53.17 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 861 | 36.52 | 11.59 | 0.57 |
| 3| 1001 | 38.58 | 12.82 | 0.60 |
| 5| 1347 | 44.07 | 15.71 | 0.68 |
| 10| 2043 | 54.81 | 22.03 | 0.84 |
| 29| 4905 | 99.38 | 47.11 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.05 | 9.07 | 0.69 |
| 2| 5935 | 35.80 | 12.02 | 0.79 |
| 3| 6041 | 43.64 | 14.62 | 0.87 |
| 4| 6186 | 53.90 | 18.17 | 0.99 |
| 5| 6360 | 60.18 | 20.24 | 1.06 |
| 6| 6610 | 74.13 | 24.97 | 1.22 |
| 7| 6721 | 81.01 | 27.30 | 1.30 |
| 8| 6668 | 84.14 | 28.18 | 1.33 |
| 9| 6985 | 98.37 | 33.07 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6511 | 60.87 | 22.83 | 1.09 |
| 10 | 30 | 1706 | 6852 | 81.11 | 30.83 | 1.33 |
| 10 | 39 | 2220 | 7159 | 99.38 | 38.04 | 1.54 |

