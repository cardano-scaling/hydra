--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-01 13:32:30.531720571 UTC |
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
| 1| 5836 | 10.61 | 3.37 | 0.52 |
| 2| 6035 | 12.99 | 4.13 | 0.55 |
| 3| 6238 | 14.40 | 4.55 | 0.57 |
| 5| 6640 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14279 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 112 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 171 | 751 | 42.54 | 12.22 | 0.62 |
| 4 | 227 | 858 | 48.08 | 13.96 | 0.68 |
| 5 | 282 | 974 | 56.81 | 16.41 | 0.77 |
| 6 | 340 | 1085 | 75.11 | 21.18 | 0.96 |
| 7 | 394 | 1192 | 82.41 | 23.36 | 1.04 |
| 8 | 450 | 1303 | 91.97 | 26.09 | 1.14 |
| 9 | 505 | 1414 | 98.44 | 28.04 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.29 | 7.69 | 0.48 |
| 2| 1959 | 25.85 | 8.81 | 0.51 |
| 3| 2145 | 28.89 | 10.32 | 0.55 |
| 5| 2325 | 30.25 | 12.02 | 0.58 |
| 10| 3252 | 43.18 | 18.96 | 0.78 |
| 40| 7569 | 95.66 | 53.59 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.84 | 7.40 | 0.42 |
| 2| 726 | 22.60 | 7.95 | 0.42 |
| 3| 836 | 24.09 | 9.03 | 0.45 |
| 5| 1265 | 30.74 | 12.27 | 0.54 |
| 10| 1925 | 38.33 | 17.70 | 0.67 |
| 40| 6434 | 97.30 | 54.05 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.50 | 8.46 | 0.46 |
| 2| 793 | 30.94 | 10.07 | 0.51 |
| 3| 906 | 30.15 | 10.52 | 0.51 |
| 5| 1289 | 35.04 | 13.25 | 0.59 |
| 10| 2017 | 47.74 | 20.13 | 0.77 |
| 36| 6053 | 99.07 | 51.95 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 696 | 33.83 | 10.15 | 0.53 |
| 2| 859 | 36.48 | 11.58 | 0.57 |
| 3| 899 | 37.20 | 12.40 | 0.58 |
| 5| 1246 | 42.65 | 15.28 | 0.66 |
| 10| 2000 | 53.91 | 21.77 | 0.83 |
| 30| 4899 | 98.08 | 47.37 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5827 | 26.96 | 9.06 | 0.69 |
| 2| 5913 | 32.44 | 10.84 | 0.75 |
| 3| 6161 | 45.68 | 15.41 | 0.90 |
| 4| 6249 | 53.80 | 18.10 | 0.99 |
| 5| 6472 | 62.53 | 21.07 | 1.09 |
| 6| 6570 | 72.32 | 24.36 | 1.20 |
| 7| 6783 | 82.58 | 27.87 | 1.32 |
| 8| 6951 | 94.80 | 32.03 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1706 | 6853 | 79.15 | 30.16 | 1.31 |
| 10 | 39 | 2220 | 7159 | 99.82 | 38.19 | 1.55 |

