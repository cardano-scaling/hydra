--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-03 10:22:47.520296757 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6035 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 15.24 | 4.85 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14285 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 32.23 | 9.37 | 0.51 |
| 3 | 171 | 747 | 43.56 | 12.48 | 0.63 |
| 4 | 226 | 858 | 51.10 | 14.71 | 0.71 |
| 5 | 282 | 969 | 59.66 | 17.16 | 0.80 |
| 6 | 337 | 1081 | 71.51 | 20.35 | 0.92 |
| 7 | 394 | 1192 | 81.28 | 23.18 | 1.03 |
| 8 | 451 | 1303 | 93.80 | 26.53 | 1.16 |
| 9 | 506 | 1414 | 89.09 | 25.80 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 23.30 | 7.41 | 0.47 |
| 2| 1882 | 24.47 | 8.41 | 0.49 |
| 3| 2013 | 26.31 | 9.58 | 0.52 |
| 5| 2402 | 31.30 | 12.31 | 0.60 |
| 10| 3165 | 41.22 | 18.41 | 0.76 |
| 42| 7839 | 99.86 | 56.04 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.77 | 7.36 | 0.42 |
| 2| 769 | 24.08 | 8.41 | 0.44 |
| 3| 903 | 25.10 | 9.32 | 0.46 |
| 5| 1149 | 28.60 | 11.68 | 0.52 |
| 10| 1996 | 38.43 | 17.72 | 0.68 |
| 41| 6485 | 94.28 | 53.93 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 27.51 | 8.47 | 0.46 |
| 2| 732 | 30.20 | 9.84 | 0.50 |
| 3| 868 | 32.04 | 11.02 | 0.53 |
| 5| 1283 | 35.08 | 13.26 | 0.59 |
| 10| 1882 | 45.98 | 19.60 | 0.75 |
| 35| 5711 | 94.15 | 49.87 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 670 | 33.83 | 10.16 | 0.53 |
| 2| 807 | 35.88 | 11.39 | 0.56 |
| 3| 1027 | 38.62 | 12.83 | 0.60 |
| 5| 1302 | 43.27 | 15.47 | 0.67 |
| 10| 1974 | 53.24 | 21.56 | 0.82 |
| 28| 4834 | 97.64 | 45.97 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5797 | 27.16 | 9.13 | 0.69 |
| 2| 5894 | 34.80 | 11.65 | 0.77 |
| 3| 5943 | 40.44 | 13.47 | 0.84 |
| 4| 6212 | 52.96 | 17.76 | 0.98 |
| 5| 6477 | 65.53 | 22.10 | 1.12 |
| 6| 6296 | 61.37 | 20.47 | 1.07 |
| 7| 6754 | 85.05 | 28.70 | 1.34 |
| 8| 6902 | 92.55 | 31.18 | 1.43 |
| 9| 6784 | 93.39 | 31.34 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.52 | 6.86 | 0.62 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6175 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1139 | 6513 | 61.05 | 22.90 | 1.10 |
| 10 | 30 | 1705 | 6851 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2218 | 7158 | 99.38 | 38.04 | 1.54 |

