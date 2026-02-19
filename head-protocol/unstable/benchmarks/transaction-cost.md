--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-19 16:24:33.07063177 UTC |
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
| 1| 5837 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.34 | 3.90 | 0.54 |
| 3| 6242 | 14.29 | 4.51 | 0.57 |
| 5| 6638 | 19.26 | 6.10 | 0.64 |
| 10| 7644 | 29.14 | 9.19 | 0.79 |
| 43| 14279 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 915 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 170 | 747 | 41.29 | 11.92 | 0.60 |
| 4 | 225 | 858 | 49.41 | 14.28 | 0.69 |
| 5 | 282 | 969 | 64.21 | 18.18 | 0.84 |
| 6 | 338 | 1081 | 71.97 | 20.51 | 0.93 |
| 7 | 395 | 1192 | 82.89 | 23.52 | 1.04 |
| 8 | 449 | 1303 | 95.29 | 26.83 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.00 | 7.62 | 0.48 |
| 2| 1928 | 25.80 | 8.77 | 0.51 |
| 3| 2197 | 29.05 | 10.36 | 0.56 |
| 5| 2273 | 28.97 | 11.67 | 0.57 |
| 10| 3090 | 39.96 | 18.06 | 0.74 |
| 42| 7888 | 99.86 | 56.09 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 609 | 22.57 | 7.31 | 0.41 |
| 2| 766 | 24.28 | 8.45 | 0.44 |
| 3| 923 | 25.03 | 9.30 | 0.46 |
| 5| 1145 | 28.14 | 11.52 | 0.51 |
| 10| 1847 | 36.69 | 17.24 | 0.65 |
| 45| 6818 | 98.12 | 57.63 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 27.54 | 8.47 | 0.47 |
| 2| 854 | 29.94 | 9.83 | 0.50 |
| 3| 915 | 32.72 | 11.23 | 0.54 |
| 5| 1283 | 35.05 | 13.25 | 0.59 |
| 10| 1939 | 46.61 | 19.79 | 0.75 |
| 35| 6046 | 97.59 | 50.90 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.15 | 9.95 | 0.52 |
| 2| 761 | 35.17 | 11.17 | 0.55 |
| 3| 942 | 37.87 | 12.61 | 0.59 |
| 5| 1286 | 43.20 | 15.45 | 0.67 |
| 10| 2175 | 55.41 | 22.22 | 0.85 |
| 28| 4771 | 96.73 | 45.69 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5812 | 26.92 | 9.04 | 0.69 |
| 2| 5974 | 36.81 | 12.42 | 0.80 |
| 3| 6131 | 45.70 | 15.41 | 0.90 |
| 4| 6232 | 53.97 | 18.12 | 0.99 |
| 5| 6413 | 64.64 | 21.81 | 1.11 |
| 6| 6536 | 70.91 | 23.86 | 1.18 |
| 7| 6726 | 81.14 | 27.34 | 1.30 |
| 8| 6708 | 83.78 | 28.09 | 1.32 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 56 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1708 | 6854 | 81.11 | 30.83 | 1.33 |
| 10 | 37 | 2102 | 7088 | 93.25 | 35.73 | 1.47 |

