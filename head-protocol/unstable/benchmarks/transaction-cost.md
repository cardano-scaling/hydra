--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-25 04:52:19.661620984 UTC |
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
| 1| 5836 | 10.36 | 3.28 | 0.51 |
| 2| 6039 | 12.23 | 3.86 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 29.09 | 9.17 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 747 | 41.12 | 11.90 | 0.60 |
| 4 | 228 | 858 | 51.12 | 14.69 | 0.71 |
| 5 | 282 | 969 | 62.35 | 17.77 | 0.82 |
| 6 | 339 | 1085 | 64.78 | 18.78 | 0.86 |
| 7 | 397 | 1192 | 81.46 | 23.27 | 1.03 |
| 8 | 450 | 1303 | 85.33 | 24.55 | 1.07 |
| 9 | 505 | 1414 | 99.22 | 28.34 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.00 | 7.62 | 0.48 |
| 2| 1949 | 25.85 | 8.78 | 0.51 |
| 3| 2056 | 27.31 | 9.86 | 0.53 |
| 5| 2275 | 28.97 | 11.67 | 0.57 |
| 10| 3148 | 40.69 | 18.28 | 0.75 |
| 38| 7530 | 97.29 | 52.68 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 627 | 22.81 | 7.37 | 0.42 |
| 2| 845 | 25.41 | 8.76 | 0.46 |
| 3| 892 | 25.09 | 9.32 | 0.46 |
| 5| 1115 | 27.01 | 11.19 | 0.50 |
| 10| 2063 | 40.64 | 18.35 | 0.70 |
| 41| 6541 | 97.75 | 54.89 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 27.54 | 8.47 | 0.46 |
| 2| 740 | 30.27 | 9.86 | 0.50 |
| 3| 953 | 33.40 | 11.44 | 0.54 |
| 5| 1390 | 36.98 | 13.85 | 0.61 |
| 10| 2036 | 44.93 | 19.37 | 0.74 |
| 36| 5997 | 98.37 | 51.75 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 816 | 35.85 | 11.38 | 0.56 |
| 3| 948 | 37.80 | 12.59 | 0.59 |
| 5| 1241 | 42.15 | 15.13 | 0.65 |
| 10| 2052 | 54.58 | 21.96 | 0.84 |
| 30| 4899 | 98.40 | 47.43 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 27.08 | 9.09 | 0.69 |
| 2| 6029 | 36.97 | 12.45 | 0.80 |
| 3| 6137 | 45.85 | 15.45 | 0.90 |
| 4| 6233 | 53.44 | 17.99 | 0.99 |
| 5| 6586 | 65.73 | 22.25 | 1.13 |
| 6| 6507 | 72.21 | 24.29 | 1.20 |
| 7| 6712 | 81.70 | 27.56 | 1.30 |
| 8| 6882 | 89.62 | 30.28 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.86 | 5.96 | 0.59 |
| 10 | 5 | 284 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 59.10 | 22.22 | 1.07 |
| 10 | 39 | 2218 | 7158 | 99.38 | 38.04 | 1.54 |

