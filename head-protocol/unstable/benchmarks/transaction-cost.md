--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 09:50:54.212770743 UTC |
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
| 1| 5837 | 10.26 | 3.25 | 0.51 |
| 2| 6042 | 12.92 | 4.11 | 0.55 |
| 3| 6239 | 14.67 | 4.64 | 0.58 |
| 5| 6641 | 18.71 | 5.91 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 99.49 | 31.12 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10050 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.40 | 9.67 | 0.52 |
| 3 | 170 | 751 | 42.73 | 12.31 | 0.62 |
| 4 | 228 | 858 | 53.57 | 15.30 | 0.73 |
| 5 | 283 | 969 | 57.82 | 16.71 | 0.78 |
| 6 | 339 | 1081 | 75.33 | 21.27 | 0.96 |
| 7 | 395 | 1196 | 74.90 | 21.61 | 0.96 |
| 8 | 451 | 1303 | 98.80 | 27.78 | 1.21 |
| 9 | 505 | 1414 | 99.73 | 28.53 | 1.22 |
| 10 | 560 | 1525 | 97.06 | 28.11 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.37 | 7.71 | 0.48 |
| 2| 1955 | 25.76 | 8.76 | 0.51 |
| 3| 2073 | 27.28 | 9.85 | 0.53 |
| 5| 2519 | 33.63 | 12.96 | 0.63 |
| 10| 3140 | 40.51 | 18.23 | 0.75 |
| 40| 7629 | 97.69 | 54.14 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 630 | 22.81 | 7.37 | 0.42 |
| 2| 726 | 22.60 | 7.95 | 0.42 |
| 3| 1034 | 28.08 | 10.18 | 0.50 |
| 5| 1228 | 29.97 | 12.03 | 0.53 |
| 10| 1931 | 38.39 | 17.72 | 0.67 |
| 38| 6160 | 96.29 | 52.46 | 1.58 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.54 | 8.47 | 0.46 |
| 2| 782 | 30.91 | 10.06 | 0.51 |
| 3| 868 | 32.01 | 11.01 | 0.53 |
| 5| 1160 | 33.70 | 12.84 | 0.57 |
| 10| 2010 | 47.56 | 20.07 | 0.77 |
| 37| 5835 | 96.97 | 51.90 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 671 | 33.87 | 10.16 | 0.53 |
| 2| 768 | 35.17 | 11.17 | 0.55 |
| 3| 939 | 37.84 | 12.60 | 0.59 |
| 5| 1255 | 42.53 | 15.25 | 0.66 |
| 10| 1921 | 53.34 | 21.59 | 0.82 |
| 29| 4874 | 97.19 | 46.47 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5834 | 27.05 | 9.08 | 0.69 |
| 2| 6002 | 36.95 | 12.48 | 0.80 |
| 3| 6226 | 47.15 | 15.93 | 0.92 |
| 4| 6213 | 54.12 | 18.17 | 0.99 |
| 5| 6326 | 59.35 | 19.93 | 1.05 |
| 6| 6486 | 72.20 | 24.25 | 1.19 |
| 7| 6646 | 82.72 | 27.82 | 1.31 |
| 8| 6813 | 90.99 | 30.63 | 1.41 |
| 9| 6803 | 94.66 | 31.86 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 284 | 6003 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 568 | 6172 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1139 | 6513 | 60.42 | 22.68 | 1.09 |
| 10 | 36 | 2052 | 7060 | 91.46 | 35.01 | 1.45 |

