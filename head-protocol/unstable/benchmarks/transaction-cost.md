--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 20:21:59.017565585 UTC |
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
| 2| 6038 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.90 | 4.72 | 0.58 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.00 | 9.14 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.19 | 9.36 | 0.51 |
| 3 | 170 | 747 | 41.34 | 11.94 | 0.60 |
| 4 | 225 | 862 | 53.70 | 15.30 | 0.73 |
| 5 | 283 | 969 | 57.62 | 16.63 | 0.78 |
| 6 | 339 | 1081 | 75.26 | 21.33 | 0.96 |
| 7 | 393 | 1192 | 86.59 | 24.36 | 1.08 |
| 8 | 448 | 1303 | 82.84 | 23.95 | 1.05 |
| 9 | 504 | 1418 | 95.78 | 27.34 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1803 | 24.37 | 7.71 | 0.48 |
| 2| 1946 | 25.88 | 8.79 | 0.51 |
| 3| 2149 | 28.02 | 10.07 | 0.54 |
| 5| 2370 | 31.19 | 12.29 | 0.60 |
| 10| 3182 | 42.32 | 18.73 | 0.77 |
| 40| 7571 | 96.15 | 53.66 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.80 | 7.37 | 0.41 |
| 2| 695 | 22.62 | 7.96 | 0.42 |
| 3| 874 | 25.82 | 9.54 | 0.47 |
| 5| 1210 | 29.22 | 11.81 | 0.52 |
| 10| 1898 | 36.83 | 17.27 | 0.66 |
| 40| 6653 | 99.98 | 54.86 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 833 | 29.19 | 9.60 | 0.49 |
| 3| 1000 | 31.65 | 10.97 | 0.53 |
| 5| 1424 | 36.91 | 13.83 | 0.61 |
| 10| 2094 | 45.30 | 19.51 | 0.75 |
| 33| 5284 | 93.58 | 48.26 | 1.49 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.83 | 10.16 | 0.53 |
| 2| 873 | 36.64 | 11.62 | 0.57 |
| 3| 988 | 38.58 | 12.82 | 0.60 |
| 5| 1248 | 42.45 | 15.23 | 0.66 |
| 10| 2208 | 56.16 | 22.45 | 0.86 |
| 29| 4803 | 96.94 | 46.35 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 27.05 | 9.07 | 0.69 |
| 2| 5958 | 37.09 | 12.51 | 0.80 |
| 3| 6188 | 45.94 | 15.46 | 0.91 |
| 4| 6214 | 54.02 | 18.14 | 0.99 |
| 5| 6359 | 60.35 | 20.29 | 1.06 |
| 6| 6637 | 75.41 | 25.53 | 1.24 |
| 7| 6587 | 78.08 | 26.28 | 1.26 |
| 8| 6845 | 90.34 | 30.46 | 1.40 |
| 9| 6959 | 97.98 | 32.92 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 56 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 568 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1705 | 6851 | 81.37 | 30.91 | 1.33 |
| 10 | 39 | 2223 | 7163 | 98.68 | 37.80 | 1.54 |

