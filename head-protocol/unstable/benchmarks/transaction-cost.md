--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-07 04:49:25.841775006 UTC |
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
| 1| 5836 | 10.28 | 3.25 | 0.51 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14279 | 98.75 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10043 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 640 | 33.25 | 9.61 | 0.52 |
| 3 | 170 | 747 | 40.09 | 11.66 | 0.59 |
| 4 | 227 | 858 | 49.43 | 14.26 | 0.69 |
| 5 | 283 | 969 | 59.35 | 17.05 | 0.80 |
| 6 | 337 | 1081 | 67.46 | 19.35 | 0.88 |
| 7 | 393 | 1192 | 78.67 | 22.51 | 1.00 |
| 8 | 449 | 1303 | 96.31 | 27.13 | 1.18 |
| 9 | 506 | 1414 | 91.34 | 26.34 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.00 | 7.62 | 0.48 |
| 2| 1995 | 26.88 | 9.07 | 0.52 |
| 3| 2084 | 27.02 | 9.79 | 0.53 |
| 5| 2323 | 30.49 | 12.08 | 0.59 |
| 10| 3094 | 39.69 | 17.99 | 0.74 |
| 42| 7799 | 99.97 | 56.10 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 864 | 25.41 | 8.77 | 0.46 |
| 3| 908 | 25.10 | 9.32 | 0.46 |
| 5| 1175 | 28.16 | 11.51 | 0.51 |
| 10| 1996 | 39.70 | 18.10 | 0.69 |
| 39| 6368 | 98.43 | 53.67 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 29.17 | 8.91 | 0.48 |
| 2| 823 | 29.26 | 9.62 | 0.49 |
| 3| 965 | 33.40 | 11.44 | 0.55 |
| 5| 1238 | 37.06 | 13.79 | 0.60 |
| 10| 1965 | 46.65 | 19.81 | 0.76 |
| 37| 6094 | 98.83 | 52.52 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 33.87 | 10.16 | 0.53 |
| 2| 806 | 35.89 | 11.39 | 0.56 |
| 3| 948 | 37.91 | 12.62 | 0.59 |
| 5| 1255 | 42.68 | 15.29 | 0.66 |
| 10| 1967 | 53.23 | 21.56 | 0.82 |
| 29| 4952 | 99.71 | 47.23 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5846 | 31.44 | 10.45 | 0.74 |
| 3| 6139 | 46.15 | 15.56 | 0.91 |
| 4| 6227 | 50.85 | 17.11 | 0.96 |
| 5| 6455 | 64.89 | 21.91 | 1.12 |
| 6| 6534 | 70.49 | 23.69 | 1.18 |
| 7| 6817 | 84.76 | 28.61 | 1.34 |
| 8| 6725 | 85.50 | 28.69 | 1.34 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1136 | 6510 | 60.87 | 22.83 | 1.09 |
| 10 | 38 | 2163 | 7125 | 95.56 | 36.62 | 1.50 |

