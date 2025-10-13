--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-13 04:41:32.524058724 UTC |
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
| 1| 5841 | 10.57 | 3.36 | 0.52 |
| 2| 6035 | 12.84 | 4.08 | 0.55 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 29.26 | 9.23 | 0.79 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.30 | 9.88 | 0.53 |
| 3 | 171 | 747 | 41.47 | 11.99 | 0.60 |
| 4 | 228 | 858 | 51.71 | 14.80 | 0.71 |
| 5 | 282 | 969 | 59.43 | 17.10 | 0.80 |
| 6 | 337 | 1085 | 68.27 | 19.62 | 0.89 |
| 7 | 393 | 1192 | 86.97 | 24.54 | 1.08 |
| 8 | 451 | 1303 | 85.79 | 24.72 | 1.08 |
| 9 | 505 | 1414 | 93.50 | 26.86 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1825 | 24.00 | 7.62 | 0.48 |
| 2| 1978 | 26.47 | 8.98 | 0.52 |
| 3| 2101 | 28.02 | 10.07 | 0.54 |
| 5| 2380 | 31.05 | 12.25 | 0.59 |
| 10| 3124 | 40.61 | 18.25 | 0.75 |
| 38| 7465 | 96.23 | 52.41 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.54 | 7.30 | 0.41 |
| 2| 817 | 25.53 | 8.79 | 0.46 |
| 3| 883 | 25.58 | 9.50 | 0.46 |
| 5| 1204 | 29.15 | 11.79 | 0.52 |
| 10| 2062 | 41.41 | 18.55 | 0.71 |
| 40| 6661 | 99.24 | 54.67 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 716 | 27.54 | 8.47 | 0.47 |
| 2| 863 | 31.66 | 10.29 | 0.52 |
| 3| 958 | 33.44 | 11.45 | 0.55 |
| 5| 1226 | 37.05 | 13.78 | 0.60 |
| 10| 2057 | 48.60 | 20.41 | 0.78 |
| 36| 5710 | 93.89 | 50.39 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 705 | 33.83 | 10.15 | 0.53 |
| 2| 837 | 35.89 | 11.39 | 0.56 |
| 3| 1006 | 38.66 | 12.84 | 0.60 |
| 5| 1263 | 42.57 | 15.26 | 0.66 |
| 10| 2019 | 54.17 | 21.85 | 0.83 |
| 29| 4929 | 99.39 | 47.11 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 26.97 | 9.07 | 0.69 |
| 2| 5958 | 36.89 | 12.44 | 0.80 |
| 3| 6129 | 45.70 | 15.44 | 0.90 |
| 4| 6411 | 56.31 | 19.12 | 1.03 |
| 5| 6459 | 65.17 | 22.01 | 1.12 |
| 6| 6605 | 74.57 | 25.12 | 1.22 |
| 7| 6787 | 81.05 | 27.35 | 1.30 |
| 8| 6663 | 82.73 | 27.78 | 1.31 |
| 9| 7008 | 98.03 | 32.97 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 56 | 5868 | 21.66 | 7.36 | 0.64 |
| 10 | 5 | 283 | 6002 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1139 | 6514 | 60.42 | 22.68 | 1.09 |
| 10 | 38 | 2163 | 7126 | 96.44 | 36.92 | 1.51 |

