--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-08 09:44:21.764054696 UTC |
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
| 1| 5836 | 10.47 | 3.32 | 0.52 |
| 2| 6041 | 12.34 | 3.90 | 0.54 |
| 3| 6239 | 14.59 | 4.61 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14279 | 99.51 | 31.12 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10061 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.62 | 0.52 |
| 3 | 169 | 747 | 43.64 | 12.50 | 0.63 |
| 4 | 226 | 858 | 48.14 | 13.98 | 0.68 |
| 5 | 282 | 969 | 58.41 | 16.89 | 0.79 |
| 6 | 339 | 1085 | 74.05 | 21.08 | 0.95 |
| 7 | 395 | 1196 | 76.61 | 22.05 | 0.98 |
| 8 | 450 | 1303 | 82.85 | 23.86 | 1.05 |
| 9 | 505 | 1414 | 95.42 | 27.60 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1792 | 23.92 | 7.60 | 0.48 |
| 2| 1924 | 25.43 | 8.68 | 0.50 |
| 3| 2055 | 27.32 | 9.86 | 0.53 |
| 5| 2371 | 31.38 | 12.33 | 0.60 |
| 10| 3222 | 41.66 | 18.55 | 0.76 |
| 40| 7525 | 98.52 | 54.30 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 611 | 22.84 | 7.39 | 0.41 |
| 2| 860 | 25.33 | 8.74 | 0.46 |
| 3| 948 | 26.88 | 9.84 | 0.48 |
| 5| 1156 | 28.04 | 11.48 | 0.51 |
| 10| 2087 | 41.24 | 18.51 | 0.71 |
| 45| 6847 | 98.41 | 57.71 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 26.83 | 8.26 | 0.45 |
| 2| 836 | 29.22 | 9.61 | 0.49 |
| 3| 925 | 32.76 | 11.24 | 0.54 |
| 5| 1279 | 37.61 | 13.96 | 0.61 |
| 10| 1946 | 46.88 | 19.88 | 0.76 |
| 36| 6031 | 98.60 | 51.77 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 699 | 33.87 | 10.16 | 0.53 |
| 2| 815 | 35.92 | 11.40 | 0.56 |
| 3| 997 | 38.66 | 12.84 | 0.60 |
| 5| 1272 | 42.49 | 15.24 | 0.66 |
| 10| 2013 | 54.09 | 21.82 | 0.83 |
| 28| 4884 | 98.26 | 46.15 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.09 | 9.10 | 0.69 |
| 2| 6002 | 36.97 | 12.46 | 0.80 |
| 3| 6137 | 44.67 | 15.03 | 0.89 |
| 4| 6241 | 54.13 | 18.19 | 0.99 |
| 5| 6365 | 58.23 | 19.58 | 1.04 |
| 6| 6481 | 69.90 | 23.54 | 1.17 |
| 7| 6813 | 84.48 | 28.57 | 1.34 |
| 8| 6802 | 88.33 | 29.71 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 570 | 6175 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2220 | 7159 | 98.05 | 37.58 | 1.53 |

