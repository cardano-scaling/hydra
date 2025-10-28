--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 12:11:42.21255654 UTC |
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
| 1| 5834 | 10.61 | 3.37 | 0.52 |
| 2| 6041 | 12.46 | 3.94 | 0.55 |
| 3| 6242 | 14.52 | 4.59 | 0.58 |
| 5| 6645 | 18.83 | 5.95 | 0.64 |
| 10| 7650 | 29.55 | 9.33 | 0.79 |
| 43| 14281 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10042 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 43.88 | 12.56 | 0.63 |
| 4 | 226 | 858 | 50.89 | 14.63 | 0.70 |
| 5 | 281 | 974 | 59.64 | 17.13 | 0.80 |
| 6 | 340 | 1085 | 75.08 | 21.25 | 0.96 |
| 7 | 396 | 1192 | 84.13 | 23.81 | 1.05 |
| 8 | 449 | 1303 | 82.62 | 23.80 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1806 | 24.00 | 7.62 | 0.48 |
| 2| 1926 | 25.51 | 8.70 | 0.50 |
| 3| 2102 | 28.30 | 10.14 | 0.54 |
| 5| 2345 | 30.18 | 12.00 | 0.58 |
| 10| 3106 | 39.51 | 17.95 | 0.74 |
| 41| 7748 | 99.96 | 55.40 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.54 | 7.30 | 0.41 |
| 2| 827 | 25.41 | 8.76 | 0.46 |
| 3| 943 | 26.56 | 9.77 | 0.48 |
| 5| 1205 | 29.19 | 11.80 | 0.52 |
| 10| 1913 | 37.44 | 17.44 | 0.66 |
| 40| 6571 | 98.31 | 54.39 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 648 | 29.13 | 8.90 | 0.48 |
| 2| 812 | 30.95 | 10.07 | 0.51 |
| 3| 915 | 32.76 | 11.24 | 0.54 |
| 5| 1219 | 34.37 | 13.04 | 0.58 |
| 10| 2032 | 44.97 | 19.39 | 0.74 |
| 38| 6252 | 99.85 | 53.45 | 1.62 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.15 | 0.53 |
| 2| 803 | 35.92 | 11.40 | 0.56 |
| 3| 990 | 38.66 | 12.84 | 0.60 |
| 5| 1273 | 42.65 | 15.28 | 0.66 |
| 10| 2048 | 54.10 | 21.82 | 0.83 |
| 29| 4837 | 97.35 | 46.53 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 27.09 | 9.08 | 0.69 |
| 2| 5898 | 32.53 | 10.86 | 0.75 |
| 3| 6087 | 44.97 | 15.12 | 0.89 |
| 4| 6253 | 55.09 | 18.54 | 1.00 |
| 5| 6331 | 63.04 | 21.15 | 1.09 |
| 6| 6562 | 71.10 | 23.91 | 1.19 |
| 7| 6774 | 84.09 | 28.41 | 1.33 |
| 8| 6849 | 88.51 | 29.75 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1138 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1709 | 6855 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2221 | 7160 | 99.12 | 37.95 | 1.54 |

