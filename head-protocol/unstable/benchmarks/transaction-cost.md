--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 14:03:46.68968975 UTC |
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
| 1| 5836 | 10.72 | 3.41 | 0.52 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.81 | 4.69 | 0.58 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.80 | 9.07 | 0.78 |
| 43| 14285 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 747 | 39.89 | 11.59 | 0.59 |
| 4 | 226 | 858 | 52.27 | 14.94 | 0.72 |
| 5 | 282 | 969 | 59.76 | 17.18 | 0.80 |
| 6 | 338 | 1081 | 68.31 | 19.63 | 0.89 |
| 7 | 395 | 1192 | 74.38 | 21.44 | 0.96 |
| 8 | 450 | 1307 | 98.88 | 27.85 | 1.21 |
| 9 | 505 | 1414 | 98.94 | 28.22 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 23.92 | 7.60 | 0.48 |
| 2| 1941 | 25.51 | 8.70 | 0.50 |
| 3| 2097 | 28.10 | 10.09 | 0.54 |
| 5| 2430 | 32.60 | 12.67 | 0.61 |
| 10| 3114 | 40.47 | 18.22 | 0.75 |
| 40| 7676 | 98.08 | 54.25 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 618 | 22.53 | 7.31 | 0.41 |
| 2| 774 | 24.05 | 8.39 | 0.44 |
| 3| 958 | 26.61 | 9.77 | 0.48 |
| 5| 1177 | 28.08 | 11.49 | 0.51 |
| 10| 1953 | 38.76 | 17.81 | 0.68 |
| 41| 6606 | 99.08 | 55.23 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.13 | 8.90 | 0.48 |
| 2| 800 | 30.98 | 10.08 | 0.51 |
| 3| 899 | 30.26 | 10.55 | 0.51 |
| 5| 1164 | 33.70 | 12.84 | 0.57 |
| 10| 2047 | 47.85 | 20.18 | 0.77 |
| 37| 6155 | 99.63 | 52.77 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 33.83 | 10.16 | 0.53 |
| 2| 833 | 35.89 | 11.39 | 0.56 |
| 3| 991 | 38.66 | 12.84 | 0.60 |
| 5| 1315 | 43.28 | 15.47 | 0.67 |
| 10| 2066 | 54.85 | 22.04 | 0.84 |
| 29| 4918 | 99.21 | 47.07 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 26.92 | 9.04 | 0.69 |
| 2| 5965 | 35.92 | 12.07 | 0.79 |
| 3| 5970 | 40.28 | 13.44 | 0.84 |
| 4| 6213 | 53.78 | 18.05 | 0.99 |
| 5| 6478 | 65.13 | 21.99 | 1.12 |
| 6| 6744 | 76.37 | 25.83 | 1.25 |
| 7| 6611 | 74.75 | 25.07 | 1.23 |
| 8| 6793 | 85.34 | 28.75 | 1.35 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 569 | 6173 | 39.95 | 14.60 | 0.85 |
| 10 | 30 | 1708 | 6854 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2220 | 7160 | 98.05 | 37.58 | 1.53 |

