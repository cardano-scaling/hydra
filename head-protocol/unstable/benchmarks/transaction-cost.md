--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-27 15:56:13.716306864 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6035 | 12.41 | 3.92 | 0.54 |
| 3| 6239 | 14.38 | 4.54 | 0.57 |
| 5| 6641 | 19.02 | 6.02 | 0.64 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14279 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 169 | 747 | 42.50 | 12.22 | 0.61 |
| 4 | 224 | 858 | 51.15 | 14.74 | 0.71 |
| 5 | 284 | 969 | 64.33 | 18.30 | 0.84 |
| 6 | 337 | 1081 | 74.92 | 21.17 | 0.96 |
| 7 | 394 | 1192 | 72.97 | 21.19 | 0.95 |
| 8 | 451 | 1303 | 83.22 | 24.00 | 1.05 |
| 9 | 505 | 1414 | 91.47 | 26.37 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1821 | 24.29 | 7.69 | 0.48 |
| 2| 1925 | 25.80 | 8.77 | 0.51 |
| 3| 2152 | 29.42 | 10.45 | 0.56 |
| 5| 2414 | 31.00 | 12.24 | 0.60 |
| 10| 3371 | 45.20 | 19.53 | 0.80 |
| 40| 7561 | 96.78 | 53.87 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.57 | 7.32 | 0.41 |
| 2| 774 | 23.65 | 8.25 | 0.43 |
| 3| 946 | 26.10 | 9.60 | 0.47 |
| 5| 1310 | 32.51 | 12.73 | 0.56 |
| 10| 2025 | 41.04 | 18.44 | 0.70 |
| 45| 6956 | 99.93 | 58.15 | 1.68 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.54 | 8.47 | 0.46 |
| 2| 778 | 30.94 | 10.07 | 0.51 |
| 3| 964 | 33.47 | 11.46 | 0.55 |
| 5| 1186 | 36.35 | 13.57 | 0.59 |
| 10| 2013 | 44.94 | 19.38 | 0.74 |
| 34| 5692 | 98.54 | 50.34 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 799 | 35.88 | 11.39 | 0.56 |
| 3| 976 | 38.63 | 12.83 | 0.60 |
| 5| 1323 | 43.39 | 15.50 | 0.67 |
| 10| 2114 | 54.34 | 21.91 | 0.84 |
| 28| 4657 | 95.36 | 45.27 | 1.45 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5808 | 27.00 | 9.07 | 0.69 |
| 2| 5820 | 31.52 | 10.48 | 0.74 |
| 3| 6207 | 46.49 | 15.75 | 0.91 |
| 4| 6302 | 56.12 | 18.98 | 1.02 |
| 5| 6525 | 66.70 | 22.53 | 1.14 |
| 6| 6505 | 69.90 | 23.45 | 1.17 |
| 7| 6520 | 75.79 | 25.44 | 1.23 |
| 8| 6876 | 90.45 | 30.45 | 1.40 |
| 9| 6883 | 98.87 | 33.19 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 29.09 | 10.34 | 0.72 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1707 | 6853 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2217 | 7156 | 98.93 | 37.88 | 1.54 |

