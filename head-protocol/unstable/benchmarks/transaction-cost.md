--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-13 10:43:35.915677558 UTC |
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
| 1| 5836 | 10.55 | 3.35 | 0.52 |
| 2| 6035 | 12.42 | 3.93 | 0.54 |
| 3| 6236 | 14.52 | 4.59 | 0.58 |
| 5| 6638 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 29.19 | 9.21 | 0.79 |
| 43| 14281 | 99.32 | 31.06 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 916 | 4.36 | 2.33 | 0.24 |
| 5| 1281 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 751 | 39.86 | 11.58 | 0.59 |
| 4 | 225 | 858 | 49.72 | 14.35 | 0.69 |
| 5 | 284 | 969 | 64.44 | 18.27 | 0.85 |
| 6 | 338 | 1081 | 66.33 | 19.15 | 0.87 |
| 7 | 395 | 1192 | 84.69 | 23.91 | 1.06 |
| 8 | 450 | 1307 | 92.08 | 26.21 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 24.00 | 7.62 | 0.48 |
| 2| 1948 | 25.85 | 8.78 | 0.51 |
| 3| 2170 | 29.10 | 10.38 | 0.56 |
| 5| 2400 | 31.58 | 12.40 | 0.60 |
| 10| 3178 | 40.93 | 18.33 | 0.75 |
| 40| 7548 | 96.03 | 53.68 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 22.77 | 7.36 | 0.42 |
| 2| 764 | 23.51 | 8.21 | 0.43 |
| 3| 853 | 23.99 | 9.01 | 0.45 |
| 5| 1288 | 31.27 | 12.39 | 0.55 |
| 10| 1883 | 37.69 | 17.50 | 0.66 |
| 38| 6182 | 94.24 | 51.90 | 1.56 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 29.13 | 8.90 | 0.48 |
| 2| 812 | 29.26 | 9.62 | 0.49 |
| 3| 1033 | 34.14 | 11.66 | 0.56 |
| 5| 1346 | 37.77 | 14.00 | 0.62 |
| 10| 1964 | 44.04 | 19.11 | 0.73 |
| 36| 5960 | 96.90 | 51.31 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 701 | 33.83 | 10.16 | 0.53 |
| 2| 908 | 36.52 | 11.59 | 0.57 |
| 3| 940 | 37.91 | 12.62 | 0.59 |
| 5| 1328 | 43.20 | 15.46 | 0.67 |
| 10| 2012 | 53.75 | 21.73 | 0.83 |
| 30| 4927 | 99.41 | 47.74 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5843 | 26.96 | 9.06 | 0.69 |
| 2| 5845 | 31.48 | 10.46 | 0.74 |
| 3| 6149 | 45.90 | 15.46 | 0.90 |
| 4| 6264 | 55.06 | 18.54 | 1.00 |
| 5| 6500 | 64.26 | 21.74 | 1.11 |
| 6| 6506 | 73.07 | 24.63 | 1.20 |
| 7| 6688 | 82.45 | 27.73 | 1.31 |
| 8| 6931 | 95.65 | 32.35 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 20 | 1139 | 6513 | 59.98 | 22.53 | 1.08 |
| 10 | 39 | 2219 | 7158 | 97.61 | 37.43 | 1.52 |

