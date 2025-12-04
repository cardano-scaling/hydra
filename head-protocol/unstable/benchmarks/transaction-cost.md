--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-04 12:46:03.936645613 UTC |
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
| 1| 5841 | 10.78 | 3.43 | 0.52 |
| 2| 6035 | 13.01 | 4.14 | 0.55 |
| 3| 6242 | 14.52 | 4.59 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.88 | 9.10 | 0.79 |
| 43| 14285 | 99.13 | 30.99 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2182 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 41.35 | 11.98 | 0.60 |
| 4 | 226 | 858 | 49.59 | 14.32 | 0.69 |
| 5 | 281 | 969 | 58.08 | 16.78 | 0.78 |
| 6 | 338 | 1081 | 66.21 | 19.09 | 0.87 |
| 7 | 393 | 1196 | 80.77 | 23.01 | 1.02 |
| 8 | 451 | 1303 | 91.79 | 26.00 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1808 | 24.37 | 7.71 | 0.48 |
| 2| 1928 | 25.39 | 8.68 | 0.50 |
| 3| 2056 | 27.43 | 9.89 | 0.53 |
| 5| 2363 | 31.00 | 12.24 | 0.59 |
| 10| 3236 | 42.38 | 18.73 | 0.77 |
| 40| 7621 | 99.18 | 54.52 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 621 | 22.84 | 7.39 | 0.42 |
| 2| 790 | 25.20 | 8.70 | 0.45 |
| 3| 899 | 25.10 | 9.32 | 0.46 |
| 5| 1221 | 30.06 | 12.05 | 0.53 |
| 10| 2034 | 39.77 | 18.11 | 0.69 |
| 41| 6667 | 97.55 | 54.84 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 27.50 | 8.46 | 0.46 |
| 2| 812 | 29.19 | 9.60 | 0.49 |
| 3| 914 | 32.76 | 11.24 | 0.54 |
| 5| 1265 | 35.01 | 13.24 | 0.58 |
| 10| 2018 | 47.51 | 20.06 | 0.77 |
| 34| 5643 | 94.76 | 49.41 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 33.87 | 10.16 | 0.53 |
| 2| 858 | 36.52 | 11.59 | 0.57 |
| 3| 900 | 37.13 | 12.38 | 0.58 |
| 5| 1267 | 42.64 | 15.28 | 0.66 |
| 10| 2079 | 55.00 | 22.08 | 0.85 |
| 30| 4925 | 99.70 | 47.79 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 27.00 | 9.07 | 0.69 |
| 2| 5891 | 34.83 | 11.64 | 0.77 |
| 3| 6070 | 42.72 | 14.35 | 0.87 |
| 4| 6172 | 50.29 | 16.85 | 0.95 |
| 5| 6301 | 60.73 | 20.40 | 1.07 |
| 6| 6680 | 74.97 | 25.33 | 1.23 |
| 7| 6645 | 76.34 | 25.63 | 1.24 |
| 8| 6797 | 89.73 | 30.18 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 20.26 | 6.78 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 570 | 6175 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1141 | 6515 | 60.17 | 22.59 | 1.09 |
| 10 | 30 | 1707 | 6854 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2222 | 7162 | 98.24 | 37.65 | 1.53 |

