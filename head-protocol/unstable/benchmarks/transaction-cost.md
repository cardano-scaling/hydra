--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-09 04:38:17.134628458 UTC |
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
| 1| 5837 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6242 | 14.48 | 4.58 | 0.57 |
| 5| 6638 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.81 | 9.07 | 0.78 |
| 43| 14282 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1273 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10073 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 33.33 | 9.64 | 0.52 |
| 3 | 170 | 747 | 41.40 | 11.97 | 0.60 |
| 4 | 226 | 858 | 52.42 | 15.00 | 0.72 |
| 5 | 283 | 969 | 55.90 | 16.19 | 0.76 |
| 6 | 338 | 1081 | 75.60 | 21.45 | 0.96 |
| 7 | 392 | 1192 | 86.17 | 24.21 | 1.07 |
| 8 | 452 | 1307 | 94.88 | 26.89 | 1.17 |
| 9 | 505 | 1414 | 97.02 | 27.81 | 1.20 |
| 10 | 560 | 1525 | 99.10 | 28.60 | 1.22 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 24.29 | 7.69 | 0.48 |
| 2| 1945 | 25.92 | 8.80 | 0.51 |
| 3| 2146 | 28.51 | 10.19 | 0.55 |
| 5| 2380 | 31.08 | 12.26 | 0.59 |
| 10| 3176 | 42.35 | 18.74 | 0.77 |
| 41| 7654 | 97.22 | 54.64 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 22.54 | 7.31 | 0.41 |
| 2| 694 | 22.62 | 7.95 | 0.42 |
| 3| 932 | 25.10 | 9.32 | 0.46 |
| 5| 1092 | 27.00 | 11.19 | 0.50 |
| 10| 1874 | 37.54 | 17.47 | 0.66 |
| 40| 6372 | 93.71 | 53.09 | 1.57 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 26.83 | 8.26 | 0.45 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 917 | 32.72 | 11.23 | 0.54 |
| 5| 1207 | 34.33 | 13.03 | 0.57 |
| 10| 1982 | 44.37 | 19.20 | 0.73 |
| 36| 6002 | 99.27 | 51.98 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 760 | 35.21 | 11.18 | 0.55 |
| 3| 984 | 38.62 | 12.83 | 0.60 |
| 5| 1232 | 42.01 | 15.08 | 0.65 |
| 10| 2082 | 53.87 | 21.76 | 0.83 |
| 29| 4915 | 99.00 | 47.01 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.97 | 7.57 | 0.64 |
| 2| 5917 | 34.87 | 11.67 | 0.78 |
| 3| 6147 | 46.09 | 15.55 | 0.91 |
| 4| 6333 | 56.28 | 18.99 | 1.02 |
| 5| 6285 | 59.43 | 19.87 | 1.05 |
| 6| 6479 | 70.32 | 23.67 | 1.17 |
| 7| 6693 | 81.83 | 27.52 | 1.30 |
| 8| 6917 | 88.15 | 29.74 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5869 | 22.10 | 7.52 | 0.64 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 570 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 39 | 2221 | 7160 | 98.49 | 37.73 | 1.53 |

