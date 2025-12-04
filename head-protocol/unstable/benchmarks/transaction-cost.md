--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-04 10:11:30.302440271 UTC |
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
| 1| 5837 | 10.76 | 3.42 | 0.52 |
| 2| 6035 | 12.54 | 3.97 | 0.55 |
| 3| 6236 | 14.97 | 4.75 | 0.58 |
| 5| 6643 | 18.60 | 5.87 | 0.64 |
| 10| 7651 | 28.71 | 9.03 | 0.78 |
| 43| 14283 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10055 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 747 | 41.39 | 11.95 | 0.60 |
| 4 | 226 | 862 | 47.94 | 13.93 | 0.68 |
| 5 | 282 | 969 | 59.82 | 17.20 | 0.80 |
| 6 | 338 | 1081 | 70.38 | 20.16 | 0.91 |
| 7 | 393 | 1192 | 87.18 | 24.55 | 1.08 |
| 8 | 450 | 1303 | 91.79 | 26.05 | 1.14 |
| 9 | 505 | 1414 | 88.01 | 25.43 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 24.00 | 7.62 | 0.48 |
| 2| 1917 | 25.84 | 8.78 | 0.51 |
| 3| 2017 | 26.31 | 9.58 | 0.52 |
| 5| 2346 | 29.85 | 11.92 | 0.58 |
| 10| 3278 | 43.78 | 19.14 | 0.79 |
| 39| 7485 | 96.22 | 53.02 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.77 | 7.36 | 0.42 |
| 2| 835 | 25.49 | 8.78 | 0.46 |
| 3| 935 | 27.10 | 9.91 | 0.48 |
| 5| 1273 | 31.13 | 12.35 | 0.55 |
| 10| 2110 | 40.64 | 18.33 | 0.70 |
| 39| 6383 | 96.65 | 53.22 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 27.47 | 8.46 | 0.46 |
| 2| 737 | 30.27 | 9.86 | 0.50 |
| 3| 962 | 33.43 | 11.45 | 0.55 |
| 5| 1177 | 36.20 | 13.53 | 0.59 |
| 10| 1990 | 47.44 | 20.04 | 0.77 |
| 34| 5694 | 92.53 | 48.75 | 1.50 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 663 | 33.83 | 10.16 | 0.53 |
| 2| 827 | 35.81 | 11.37 | 0.56 |
| 3| 933 | 37.88 | 12.61 | 0.59 |
| 5| 1351 | 44.11 | 15.72 | 0.68 |
| 10| 1991 | 54.29 | 21.86 | 0.83 |
| 29| 4853 | 97.73 | 46.62 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 26.96 | 9.05 | 0.69 |
| 2| 5868 | 32.54 | 10.86 | 0.75 |
| 3| 6156 | 45.77 | 15.41 | 0.90 |
| 4| 6158 | 51.66 | 17.34 | 0.96 |
| 5| 6462 | 62.77 | 21.19 | 1.09 |
| 6| 6545 | 74.75 | 25.20 | 1.22 |
| 7| 6593 | 74.85 | 25.14 | 1.23 |
| 8| 6869 | 93.79 | 31.63 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.02 | 9.98 | 0.71 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1709 | 6855 | 79.60 | 30.31 | 1.31 |
| 10 | 37 | 2108 | 7094 | 94.83 | 36.27 | 1.49 |

