--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-27 04:51:14.605713029 UTC |
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
| 1| 5834 | 10.48 | 3.33 | 0.52 |
| 2| 6035 | 12.32 | 3.89 | 0.54 |
| 3| 6243 | 14.50 | 4.58 | 0.58 |
| 5| 6638 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 58 | 526 | 25.20 | 7.30 | 0.43 |
| 2 | 112 | 635 | 32.19 | 9.36 | 0.51 |
| 3 | 171 | 747 | 41.24 | 11.91 | 0.60 |
| 4 | 227 | 858 | 49.55 | 14.36 | 0.69 |
| 5 | 283 | 969 | 60.02 | 17.25 | 0.80 |
| 6 | 339 | 1081 | 71.25 | 20.29 | 0.92 |
| 7 | 394 | 1192 | 72.11 | 20.85 | 0.94 |
| 8 | 449 | 1307 | 96.03 | 27.02 | 1.18 |
| 9 | 506 | 1414 | 96.85 | 27.72 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1783 | 24.29 | 7.69 | 0.48 |
| 2| 1955 | 25.88 | 8.79 | 0.51 |
| 3| 2080 | 27.43 | 9.89 | 0.53 |
| 5| 2359 | 31.45 | 12.35 | 0.60 |
| 10| 3106 | 40.45 | 18.20 | 0.74 |
| 41| 7767 | 99.75 | 55.38 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 22.81 | 7.37 | 0.42 |
| 2| 735 | 24.31 | 8.45 | 0.44 |
| 3| 913 | 25.45 | 9.45 | 0.46 |
| 5| 1271 | 31.31 | 12.40 | 0.55 |
| 10| 2025 | 40.28 | 18.26 | 0.70 |
| 41| 6682 | 97.98 | 54.96 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 29.17 | 8.91 | 0.48 |
| 2| 736 | 30.19 | 9.84 | 0.50 |
| 3| 925 | 32.76 | 11.24 | 0.54 |
| 5| 1262 | 35.01 | 13.24 | 0.58 |
| 10| 1978 | 44.18 | 19.15 | 0.73 |
| 36| 6011 | 97.65 | 51.48 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 33.87 | 10.16 | 0.53 |
| 2| 811 | 35.85 | 11.38 | 0.56 |
| 3| 942 | 37.84 | 12.60 | 0.59 |
| 5| 1241 | 42.68 | 15.29 | 0.66 |
| 10| 2030 | 53.75 | 21.73 | 0.83 |
| 30| 4796 | 97.33 | 47.09 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 27.00 | 9.06 | 0.69 |
| 2| 5846 | 31.44 | 10.45 | 0.74 |
| 3| 6099 | 44.51 | 14.98 | 0.89 |
| 4| 6135 | 50.83 | 17.02 | 0.95 |
| 5| 6447 | 65.31 | 22.03 | 1.12 |
| 6| 6594 | 74.52 | 25.07 | 1.22 |
| 7| 6586 | 75.86 | 25.45 | 1.24 |
| 8| 6888 | 90.43 | 30.55 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 10 | 568 | 6172 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1140 | 6515 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 80.22 | 30.52 | 1.32 |
| 10 | 40 | 2276 | 7193 | 99.66 | 38.24 | 1.55 |

