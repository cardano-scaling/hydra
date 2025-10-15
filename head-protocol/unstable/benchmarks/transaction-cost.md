--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-15 04:41:32.640550422 UTC |
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
| 1| 5834 | 10.55 | 3.35 | 0.52 |
| 2| 6035 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.52 | 4.59 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 563 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10075 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 170 | 747 | 41.12 | 11.88 | 0.60 |
| 4 | 226 | 862 | 53.85 | 15.36 | 0.73 |
| 5 | 282 | 974 | 57.90 | 16.71 | 0.78 |
| 6 | 341 | 1085 | 68.46 | 19.67 | 0.89 |
| 7 | 393 | 1192 | 80.07 | 22.80 | 1.01 |
| 8 | 450 | 1303 | 97.32 | 27.53 | 1.19 |
| 9 | 505 | 1414 | 98.93 | 28.21 | 1.22 |
| 10 | 560 | 1525 | 98.10 | 28.55 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.00 | 7.62 | 0.48 |
| 2| 1994 | 26.58 | 9.01 | 0.52 |
| 3| 2062 | 26.90 | 9.76 | 0.53 |
| 5| 2493 | 33.07 | 12.82 | 0.62 |
| 10| 3245 | 42.97 | 18.91 | 0.78 |
| 43| 7851 | 99.96 | 56.71 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.81 | 7.37 | 0.42 |
| 2| 764 | 23.59 | 8.23 | 0.43 |
| 3| 897 | 25.83 | 9.54 | 0.47 |
| 5| 1243 | 30.03 | 12.04 | 0.53 |
| 10| 2128 | 41.52 | 18.59 | 0.71 |
| 40| 6652 | 98.45 | 54.42 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 29.17 | 8.91 | 0.48 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 976 | 33.36 | 11.43 | 0.55 |
| 5| 1209 | 34.37 | 13.04 | 0.58 |
| 10| 2022 | 44.34 | 19.19 | 0.74 |
| 37| 6033 | 98.90 | 52.49 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 668 | 33.87 | 10.16 | 0.53 |
| 2| 761 | 35.17 | 11.17 | 0.55 |
| 3| 980 | 38.55 | 12.81 | 0.60 |
| 5| 1253 | 42.61 | 15.27 | 0.66 |
| 10| 1900 | 52.78 | 21.41 | 0.81 |
| 29| 4959 | 99.08 | 47.02 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.05 | 9.07 | 0.69 |
| 2| 5926 | 35.85 | 12.04 | 0.79 |
| 3| 6227 | 46.86 | 15.83 | 0.92 |
| 4| 6192 | 50.58 | 16.97 | 0.95 |
| 5| 6403 | 63.84 | 21.50 | 1.10 |
| 6| 6630 | 74.42 | 25.10 | 1.22 |
| 7| 6699 | 78.20 | 26.34 | 1.27 |
| 8| 6703 | 84.01 | 28.34 | 1.33 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 56 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.65 | 10.19 | 0.72 |
| 10 | 20 | 1138 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1709 | 6856 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2216 | 7155 | 98.93 | 37.88 | 1.54 |

