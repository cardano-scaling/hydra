--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-26 09:07:27.527431469 UTC |
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
| 2| 6037 | 13.01 | 4.14 | 0.55 |
| 3| 6238 | 15.07 | 4.78 | 0.58 |
| 5| 6641 | 18.52 | 5.84 | 0.63 |
| 10| 7651 | 29.49 | 9.31 | 0.79 |
| 43| 14281 | 99.23 | 31.02 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 556 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 747 | 41.51 | 12.02 | 0.61 |
| 4 | 228 | 858 | 51.01 | 14.66 | 0.71 |
| 5 | 283 | 969 | 62.69 | 17.85 | 0.83 |
| 6 | 337 | 1081 | 65.24 | 18.81 | 0.86 |
| 7 | 396 | 1192 | 78.02 | 22.26 | 0.99 |
| 8 | 450 | 1303 | 97.15 | 27.22 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1784 | 24.29 | 7.69 | 0.48 |
| 2| 2004 | 26.96 | 9.09 | 0.52 |
| 3| 2014 | 25.94 | 9.49 | 0.52 |
| 5| 2435 | 32.04 | 12.53 | 0.61 |
| 10| 3235 | 41.74 | 18.57 | 0.76 |
| 40| 7645 | 98.56 | 54.36 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 607 | 22.57 | 7.30 | 0.41 |
| 2| 741 | 24.35 | 8.46 | 0.44 |
| 3| 922 | 25.06 | 9.31 | 0.46 |
| 5| 1236 | 29.01 | 11.75 | 0.52 |
| 10| 2062 | 40.92 | 18.42 | 0.70 |
| 39| 6437 | 96.65 | 53.25 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 27.54 | 8.47 | 0.46 |
| 2| 875 | 29.97 | 9.84 | 0.50 |
| 3| 910 | 32.65 | 11.21 | 0.53 |
| 5| 1346 | 35.72 | 13.46 | 0.60 |
| 10| 1984 | 44.27 | 19.17 | 0.73 |
| 35| 5566 | 98.10 | 50.83 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 699 | 33.87 | 10.16 | 0.53 |
| 2| 888 | 36.52 | 11.59 | 0.57 |
| 3| 959 | 37.91 | 12.62 | 0.59 |
| 5| 1244 | 42.61 | 15.27 | 0.66 |
| 10| 1929 | 52.60 | 21.37 | 0.81 |
| 29| 4977 | 98.86 | 46.98 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.13 | 9.09 | 0.69 |
| 2| 5971 | 36.73 | 12.38 | 0.80 |
| 3| 6132 | 45.95 | 15.51 | 0.90 |
| 4| 6114 | 47.04 | 15.68 | 0.91 |
| 5| 6377 | 60.50 | 20.34 | 1.07 |
| 6| 6480 | 67.70 | 22.79 | 1.15 |
| 7| 6665 | 76.73 | 25.80 | 1.25 |
| 8| 7017 | 91.82 | 30.97 | 1.42 |
| 9| 7135 | 99.67 | 33.64 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.93 | 6.32 | 0.61 |
| 10 | 5 | 285 | 6004 | 28.65 | 10.19 | 0.72 |
| 10 | 10 | 570 | 6175 | 40.13 | 14.67 | 0.85 |
| 10 | 30 | 1707 | 6853 | 81.37 | 30.91 | 1.33 |
| 10 | 40 | 2279 | 7195 | 99.22 | 38.09 | 1.54 |
| 10 | 39 | 2219 | 7159 | 99.82 | 38.19 | 1.55 |

