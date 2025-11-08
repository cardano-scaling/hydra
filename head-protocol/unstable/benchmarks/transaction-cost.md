--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-08 04:47:25.609542128 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6042 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.76 | 4.67 | 0.58 |
| 5| 6643 | 18.84 | 5.95 | 0.64 |
| 10| 7646 | 28.80 | 9.07 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 169 | 747 | 43.84 | 12.56 | 0.63 |
| 4 | 226 | 858 | 50.95 | 14.65 | 0.71 |
| 5 | 283 | 974 | 61.45 | 17.56 | 0.82 |
| 6 | 339 | 1081 | 75.02 | 21.19 | 0.96 |
| 7 | 394 | 1192 | 74.01 | 21.43 | 0.96 |
| 8 | 451 | 1307 | 92.12 | 26.18 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1792 | 24.00 | 7.62 | 0.48 |
| 2| 1887 | 24.77 | 8.48 | 0.49 |
| 3| 2112 | 28.47 | 10.18 | 0.55 |
| 5| 2509 | 33.43 | 12.91 | 0.62 |
| 10| 3117 | 40.32 | 18.18 | 0.74 |
| 40| 7538 | 95.32 | 53.46 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 619 | 22.80 | 7.37 | 0.41 |
| 2| 789 | 24.28 | 8.45 | 0.44 |
| 3| 874 | 25.51 | 9.47 | 0.46 |
| 5| 1248 | 30.06 | 12.06 | 0.54 |
| 10| 1930 | 38.71 | 17.80 | 0.68 |
| 41| 6625 | 98.24 | 55.03 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 29.09 | 8.89 | 0.48 |
| 2| 846 | 29.18 | 9.60 | 0.49 |
| 3| 988 | 31.69 | 10.98 | 0.53 |
| 5| 1223 | 36.95 | 13.76 | 0.60 |
| 10| 2019 | 48.23 | 20.28 | 0.77 |
| 36| 5872 | 96.93 | 51.26 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 33.79 | 10.15 | 0.53 |
| 2| 810 | 35.88 | 11.39 | 0.56 |
| 3| 893 | 37.20 | 12.40 | 0.58 |
| 5| 1204 | 42.01 | 15.08 | 0.65 |
| 10| 1985 | 53.19 | 21.55 | 0.82 |
| 28| 4613 | 95.30 | 45.24 | 1.45 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5698 | 22.93 | 7.56 | 0.64 |
| 2| 5845 | 31.48 | 10.47 | 0.74 |
| 3| 6080 | 45.03 | 15.14 | 0.89 |
| 4| 6382 | 56.95 | 19.27 | 1.03 |
| 5| 6520 | 65.34 | 22.03 | 1.12 |
| 6| 6441 | 65.94 | 22.11 | 1.13 |
| 7| 6697 | 80.61 | 27.13 | 1.29 |
| 8| 6953 | 91.39 | 30.85 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 22.10 | 7.52 | 0.64 |
| 10 | 5 | 285 | 6004 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 570 | 6175 | 38.81 | 14.21 | 0.84 |
| 10 | 20 | 1138 | 6512 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1710 | 6856 | 79.15 | 30.16 | 1.31 |
| 10 | 36 | 2054 | 7062 | 92.78 | 35.46 | 1.47 |

