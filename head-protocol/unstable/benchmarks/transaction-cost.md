--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-21 12:11:07.020068536 UTC |
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
| 1| 5837 | 10.36 | 3.28 | 0.51 |
| 2| 6038 | 12.72 | 4.03 | 0.55 |
| 3| 6239 | 14.40 | 4.55 | 0.57 |
| 5| 6645 | 18.62 | 5.87 | 0.64 |
| 10| 7650 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 99.11 | 30.98 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 924 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 112 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 171 | 747 | 41.35 | 11.96 | 0.60 |
| 4 | 226 | 858 | 52.50 | 15.02 | 0.72 |
| 5 | 285 | 974 | 59.63 | 17.18 | 0.80 |
| 6 | 337 | 1085 | 71.53 | 20.40 | 0.92 |
| 7 | 395 | 1196 | 78.49 | 22.51 | 1.00 |
| 8 | 450 | 1303 | 94.48 | 26.69 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 23.92 | 7.60 | 0.48 |
| 2| 1920 | 25.51 | 8.70 | 0.50 |
| 3| 2168 | 29.42 | 10.45 | 0.56 |
| 5| 2412 | 32.57 | 12.66 | 0.61 |
| 10| 3153 | 41.06 | 18.37 | 0.75 |
| 41| 7950 | 99.92 | 55.46 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 645 | 22.50 | 7.30 | 0.41 |
| 2| 722 | 22.52 | 7.93 | 0.42 |
| 3| 923 | 25.14 | 9.33 | 0.46 |
| 5| 1207 | 30.12 | 12.09 | 0.53 |
| 10| 2031 | 41.78 | 18.66 | 0.71 |
| 44| 6813 | 97.27 | 56.77 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 661 | 29.09 | 8.89 | 0.48 |
| 2| 843 | 31.65 | 10.28 | 0.52 |
| 3| 996 | 31.65 | 10.97 | 0.53 |
| 5| 1353 | 36.36 | 13.65 | 0.60 |
| 10| 2080 | 45.61 | 19.58 | 0.75 |
| 37| 5982 | 97.84 | 52.20 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.12 | 9.94 | 0.52 |
| 2| 799 | 35.92 | 11.40 | 0.56 |
| 3| 895 | 37.16 | 12.39 | 0.58 |
| 5| 1318 | 43.36 | 15.49 | 0.67 |
| 10| 2098 | 54.78 | 22.02 | 0.84 |
| 29| 4815 | 97.98 | 46.68 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5792 | 27.09 | 9.10 | 0.69 |
| 2| 5889 | 32.52 | 10.87 | 0.75 |
| 3| 6086 | 42.45 | 14.25 | 0.86 |
| 4| 6205 | 52.47 | 17.66 | 0.97 |
| 5| 6373 | 63.78 | 21.41 | 1.10 |
| 6| 6538 | 70.41 | 23.74 | 1.18 |
| 7| 6535 | 75.41 | 25.33 | 1.23 |
| 8| 6776 | 88.16 | 29.59 | 1.37 |
| 9| 7047 | 97.13 | 32.72 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1140 | 6514 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1705 | 6851 | 80.04 | 30.46 | 1.32 |
| 10 | 40 | 2278 | 7194 | 99.66 | 38.24 | 1.55 |

