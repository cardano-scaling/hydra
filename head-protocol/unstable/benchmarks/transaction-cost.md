--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-21 04:42:11.127353495 UTC |
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
| 1| 5837 | 10.85 | 3.45 | 0.52 |
| 2| 6035 | 12.46 | 3.94 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6641 | 18.84 | 5.95 | 0.64 |
| 10| 7644 | 29.00 | 9.14 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 744 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10052 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 171 | 747 | 43.63 | 12.50 | 0.63 |
| 4 | 225 | 858 | 48.35 | 14.08 | 0.68 |
| 5 | 283 | 969 | 64.46 | 18.31 | 0.85 |
| 6 | 338 | 1081 | 73.80 | 20.98 | 0.95 |
| 7 | 394 | 1192 | 78.91 | 22.61 | 1.00 |
| 8 | 448 | 1307 | 90.14 | 25.76 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 24.00 | 7.62 | 0.48 |
| 2| 1923 | 25.43 | 8.68 | 0.50 |
| 3| 2076 | 27.32 | 9.86 | 0.53 |
| 5| 2388 | 31.07 | 12.26 | 0.59 |
| 10| 3132 | 40.30 | 18.16 | 0.74 |
| 41| 7754 | 99.53 | 55.31 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 22.54 | 7.30 | 0.41 |
| 2| 817 | 25.56 | 8.81 | 0.46 |
| 3| 830 | 24.06 | 9.03 | 0.45 |
| 5| 1266 | 30.73 | 12.26 | 0.54 |
| 10| 2084 | 42.29 | 18.79 | 0.72 |
| 41| 6599 | 98.40 | 55.09 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.54 | 8.47 | 0.46 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 967 | 33.39 | 11.43 | 0.55 |
| 5| 1169 | 36.34 | 13.57 | 0.59 |
| 10| 2104 | 48.90 | 20.48 | 0.79 |
| 37| 6070 | 98.20 | 52.31 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 873 | 36.60 | 11.61 | 0.57 |
| 3| 1067 | 39.30 | 13.04 | 0.61 |
| 5| 1331 | 43.24 | 15.46 | 0.67 |
| 10| 1959 | 53.34 | 21.59 | 0.82 |
| 29| 4948 | 99.57 | 47.14 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5780 | 26.97 | 9.05 | 0.69 |
| 2| 5914 | 36.01 | 12.09 | 0.79 |
| 3| 6068 | 44.59 | 15.01 | 0.89 |
| 4| 6257 | 55.43 | 18.66 | 1.01 |
| 5| 6310 | 59.47 | 19.95 | 1.05 |
| 6| 6483 | 72.92 | 24.51 | 1.20 |
| 7| 6805 | 86.12 | 29.10 | 1.36 |
| 8| 6948 | 95.84 | 32.33 | 1.46 |
| 9| 6850 | 97.94 | 32.91 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 56 | 5868 | 21.66 | 7.36 | 0.64 |
| 10 | 20 | 1139 | 6513 | 59.73 | 22.44 | 1.08 |
| 10 | 30 | 1705 | 6851 | 80.67 | 30.67 | 1.32 |
| 10 | 39 | 2220 | 7159 | 97.16 | 37.28 | 1.52 |

