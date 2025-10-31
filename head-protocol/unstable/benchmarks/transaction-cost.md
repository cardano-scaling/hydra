--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-31 04:50:20.974805864 UTC |
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
| 1| 5834 | 10.85 | 3.45 | 0.52 |
| 2| 6035 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 170 | 747 | 40.24 | 11.69 | 0.59 |
| 4 | 227 | 858 | 52.12 | 14.93 | 0.72 |
| 5 | 283 | 969 | 57.81 | 16.65 | 0.78 |
| 6 | 338 | 1081 | 63.89 | 18.53 | 0.85 |
| 7 | 393 | 1192 | 72.19 | 20.96 | 0.94 |
| 8 | 449 | 1307 | 81.71 | 23.79 | 1.04 |
| 9 | 505 | 1414 | 96.48 | 27.68 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 24.00 | 7.62 | 0.48 |
| 2| 1987 | 26.96 | 9.09 | 0.52 |
| 3| 2125 | 28.39 | 10.16 | 0.55 |
| 5| 2404 | 32.19 | 12.57 | 0.61 |
| 10| 3341 | 44.11 | 19.22 | 0.79 |
| 41| 7709 | 98.09 | 54.89 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.54 | 7.30 | 0.41 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 1023 | 28.17 | 10.19 | 0.50 |
| 5| 1199 | 29.08 | 11.77 | 0.52 |
| 10| 1877 | 38.37 | 17.71 | 0.67 |
| 39| 6246 | 91.42 | 51.79 | 1.54 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 27.50 | 8.46 | 0.46 |
| 2| 782 | 30.94 | 10.07 | 0.51 |
| 3| 968 | 33.40 | 11.43 | 0.55 |
| 5| 1265 | 37.66 | 13.97 | 0.61 |
| 10| 1997 | 47.59 | 20.08 | 0.77 |
| 35| 5673 | 99.39 | 51.20 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.87 | 10.16 | 0.53 |
| 2| 806 | 35.85 | 11.38 | 0.56 |
| 3| 1012 | 38.55 | 12.81 | 0.60 |
| 5| 1300 | 43.28 | 15.47 | 0.67 |
| 10| 2038 | 54.28 | 21.88 | 0.84 |
| 29| 4898 | 99.16 | 47.04 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5803 | 27.13 | 9.10 | 0.69 |
| 2| 5893 | 34.87 | 11.67 | 0.78 |
| 3| 6060 | 44.72 | 15.06 | 0.89 |
| 4| 6276 | 52.70 | 17.72 | 0.98 |
| 5| 6468 | 64.99 | 21.88 | 1.12 |
| 6| 6529 | 69.23 | 23.28 | 1.16 |
| 7| 6752 | 82.98 | 28.03 | 1.32 |
| 8| 6943 | 94.39 | 31.96 | 1.45 |
| 9| 7039 | 98.88 | 33.29 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.38 | 6.48 | 0.61 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 30 | 1710 | 6857 | 80.67 | 30.67 | 1.32 |
| 10 | 40 | 2276 | 7192 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2219 | 7159 | 97.61 | 37.43 | 1.52 |

