--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-12 15:35:11.920653856 UTC |
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
| 1| 5837 | 10.95 | 3.49 | 0.52 |
| 2| 6041 | 12.70 | 4.03 | 0.55 |
| 3| 6239 | 14.29 | 4.51 | 0.57 |
| 5| 6638 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 29.38 | 9.27 | 0.79 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 112 | 635 | 34.19 | 9.84 | 0.53 |
| 3 | 170 | 747 | 41.35 | 11.96 | 0.60 |
| 4 | 227 | 858 | 53.79 | 15.30 | 0.73 |
| 5 | 285 | 969 | 56.46 | 16.42 | 0.77 |
| 6 | 338 | 1081 | 74.72 | 21.08 | 0.95 |
| 7 | 394 | 1192 | 80.89 | 23.08 | 1.02 |
| 8 | 450 | 1303 | 79.96 | 23.11 | 1.02 |
| 9 | 507 | 1414 | 91.56 | 26.50 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.00 | 7.62 | 0.48 |
| 2| 2009 | 26.54 | 9.00 | 0.52 |
| 3| 2140 | 28.02 | 10.07 | 0.54 |
| 5| 2486 | 33.20 | 12.85 | 0.62 |
| 10| 3200 | 40.98 | 18.37 | 0.75 |
| 41| 7732 | 97.42 | 54.74 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 22.81 | 7.38 | 0.42 |
| 2| 699 | 22.58 | 7.95 | 0.42 |
| 3| 904 | 25.14 | 9.33 | 0.46 |
| 5| 1197 | 28.99 | 11.75 | 0.52 |
| 10| 2092 | 40.78 | 18.40 | 0.70 |
| 41| 6549 | 99.20 | 55.29 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 29.17 | 8.91 | 0.48 |
| 2| 884 | 29.89 | 9.82 | 0.50 |
| 3| 935 | 32.76 | 11.24 | 0.54 |
| 5| 1343 | 38.33 | 14.18 | 0.62 |
| 10| 1975 | 47.51 | 20.06 | 0.77 |
| 34| 5436 | 95.84 | 49.52 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 33.87 | 10.16 | 0.53 |
| 2| 880 | 36.56 | 11.60 | 0.57 |
| 3| 1004 | 38.66 | 12.84 | 0.60 |
| 5| 1310 | 43.24 | 15.47 | 0.67 |
| 10| 2126 | 55.36 | 22.21 | 0.85 |
| 29| 4823 | 98.52 | 46.84 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.13 | 9.10 | 0.69 |
| 2| 5960 | 35.91 | 12.05 | 0.79 |
| 3| 6137 | 44.99 | 15.11 | 0.89 |
| 4| 6326 | 56.97 | 19.29 | 1.03 |
| 5| 6429 | 61.75 | 20.81 | 1.08 |
| 6| 6409 | 72.30 | 24.20 | 1.19 |
| 7| 6678 | 82.06 | 27.70 | 1.31 |
| 8| 6941 | 92.50 | 31.08 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.49 | 6.17 | 0.60 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1137 | 6511 | 59.03 | 22.20 | 1.07 |
| 10 | 30 | 1708 | 6854 | 81.99 | 31.13 | 1.34 |
| 10 | 37 | 2105 | 7091 | 94.39 | 36.12 | 1.49 |

