--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-15 17:14:44.292114389 UTC |
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
| 1| 5836 | 10.86 | 3.46 | 0.52 |
| 2| 6042 | 12.53 | 3.97 | 0.55 |
| 3| 6239 | 14.50 | 4.58 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 29.21 | 9.21 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10045 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 32.27 | 9.39 | 0.51 |
| 3 | 169 | 747 | 42.49 | 12.21 | 0.61 |
| 4 | 227 | 858 | 52.16 | 14.91 | 0.72 |
| 5 | 283 | 969 | 64.57 | 18.30 | 0.85 |
| 6 | 337 | 1081 | 70.64 | 20.27 | 0.91 |
| 7 | 397 | 1192 | 79.04 | 22.64 | 1.00 |
| 8 | 450 | 1303 | 87.85 | 25.21 | 1.10 |
| 9 | 506 | 1418 | 93.62 | 26.89 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.29 | 7.69 | 0.48 |
| 2| 1886 | 24.48 | 8.42 | 0.49 |
| 3| 2064 | 26.95 | 9.77 | 0.53 |
| 5| 2398 | 31.41 | 12.34 | 0.60 |
| 10| 3202 | 42.24 | 18.71 | 0.77 |
| 39| 7313 | 92.30 | 51.95 | 1.59 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 22.77 | 7.36 | 0.42 |
| 2| 791 | 23.55 | 8.22 | 0.43 |
| 3| 932 | 26.98 | 9.88 | 0.48 |
| 5| 1148 | 27.96 | 11.46 | 0.51 |
| 10| 2096 | 41.84 | 18.70 | 0.72 |
| 41| 6650 | 97.25 | 54.77 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.13 | 8.90 | 0.48 |
| 2| 792 | 30.98 | 10.08 | 0.51 |
| 3| 964 | 30.87 | 10.74 | 0.52 |
| 5| 1292 | 35.08 | 13.26 | 0.59 |
| 10| 1983 | 47.36 | 20.02 | 0.76 |
| 36| 6076 | 98.81 | 51.83 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 33.83 | 10.16 | 0.53 |
| 2| 811 | 35.89 | 11.39 | 0.56 |
| 3| 942 | 37.80 | 12.59 | 0.59 |
| 5| 1295 | 43.25 | 15.47 | 0.67 |
| 10| 2079 | 54.74 | 22.01 | 0.84 |
| 29| 4767 | 96.69 | 46.28 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 27.05 | 9.07 | 0.69 |
| 2| 6015 | 37.00 | 12.47 | 0.80 |
| 3| 5993 | 43.89 | 14.71 | 0.87 |
| 4| 6138 | 50.29 | 16.84 | 0.95 |
| 5| 6312 | 57.02 | 19.11 | 1.03 |
| 6| 6716 | 76.88 | 26.06 | 1.25 |
| 7| 6786 | 85.80 | 29.02 | 1.35 |
| 8| 6912 | 94.46 | 31.87 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 284 | 6003 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 568 | 6172 | 38.81 | 14.21 | 0.84 |
| 10 | 20 | 1138 | 6513 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1708 | 6855 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2219 | 7159 | 98.93 | 37.88 | 1.54 |

