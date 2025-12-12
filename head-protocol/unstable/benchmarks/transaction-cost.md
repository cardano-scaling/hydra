--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-12 09:52:40.441654361 UTC |
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
| 1| 5834 | 10.57 | 3.36 | 0.52 |
| 2| 6041 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 19.10 | 6.05 | 0.64 |
| 10| 7647 | 29.19 | 9.21 | 0.79 |
| 43| 14281 | 99.33 | 31.06 | 1.81 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 169 | 747 | 42.35 | 12.18 | 0.61 |
| 4 | 227 | 858 | 49.43 | 14.26 | 0.69 |
| 5 | 284 | 969 | 63.79 | 18.08 | 0.84 |
| 6 | 338 | 1081 | 73.95 | 20.98 | 0.95 |
| 7 | 395 | 1192 | 76.91 | 22.09 | 0.98 |
| 8 | 449 | 1303 | 80.81 | 23.42 | 1.03 |
| 9 | 506 | 1418 | 93.79 | 26.93 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 23.92 | 7.60 | 0.48 |
| 2| 1978 | 26.55 | 9.00 | 0.52 |
| 3| 2083 | 27.10 | 9.81 | 0.53 |
| 5| 2425 | 32.25 | 12.58 | 0.61 |
| 10| 3186 | 41.71 | 18.56 | 0.76 |
| 40| 7635 | 97.61 | 54.09 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 637 | 22.54 | 7.30 | 0.41 |
| 2| 834 | 25.45 | 8.77 | 0.46 |
| 3| 898 | 25.49 | 9.46 | 0.46 |
| 5| 1204 | 29.94 | 12.02 | 0.53 |
| 10| 2007 | 40.33 | 18.25 | 0.70 |
| 41| 6656 | 98.35 | 55.06 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 648 | 29.13 | 8.90 | 0.48 |
| 2| 782 | 30.98 | 10.08 | 0.51 |
| 3| 914 | 32.79 | 11.25 | 0.54 |
| 5| 1236 | 34.26 | 13.02 | 0.58 |
| 10| 2000 | 47.22 | 19.98 | 0.76 |
| 37| 6068 | 97.90 | 52.26 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.15 | 9.95 | 0.52 |
| 2| 852 | 36.52 | 11.59 | 0.57 |
| 3| 996 | 38.62 | 12.83 | 0.60 |
| 5| 1376 | 43.83 | 15.65 | 0.68 |
| 10| 1995 | 53.45 | 21.62 | 0.83 |
| 29| 4652 | 95.97 | 46.05 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 27.08 | 9.08 | 0.69 |
| 2| 5927 | 35.93 | 12.05 | 0.79 |
| 3| 6098 | 44.54 | 14.97 | 0.89 |
| 4| 6251 | 54.80 | 18.42 | 1.00 |
| 5| 6461 | 65.12 | 21.93 | 1.12 |
| 6| 6497 | 70.94 | 23.86 | 1.18 |
| 7| 6603 | 79.38 | 26.61 | 1.27 |
| 8| 6773 | 89.58 | 30.09 | 1.39 |
| 9| 6826 | 94.31 | 31.74 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1707 | 6853 | 79.60 | 30.31 | 1.31 |
| 10 | 34 | 1935 | 6989 | 88.24 | 33.69 | 1.41 |

