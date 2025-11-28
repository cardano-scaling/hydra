--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-28 17:39:32.346328393 UTC |
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
| 1| 5836 | 10.66 | 3.39 | 0.52 |
| 2| 6037 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.59 | 4.61 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10047 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 39.97 | 11.62 | 0.59 |
| 4 | 227 | 858 | 49.66 | 14.34 | 0.69 |
| 5 | 282 | 969 | 58.50 | 16.91 | 0.79 |
| 6 | 338 | 1081 | 71.49 | 20.39 | 0.92 |
| 7 | 392 | 1196 | 82.95 | 23.54 | 1.04 |
| 8 | 450 | 1303 | 82.15 | 23.64 | 1.04 |
| 9 | 505 | 1414 | 98.21 | 27.93 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1812 | 24.37 | 7.71 | 0.48 |
| 2| 1932 | 25.55 | 8.71 | 0.50 |
| 3| 2017 | 26.32 | 9.58 | 0.52 |
| 5| 2424 | 32.33 | 12.60 | 0.61 |
| 10| 3329 | 44.46 | 19.33 | 0.80 |
| 42| 7607 | 96.42 | 55.08 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 22.81 | 7.37 | 0.42 |
| 2| 695 | 22.55 | 7.95 | 0.42 |
| 3| 999 | 27.04 | 9.88 | 0.48 |
| 5| 1254 | 30.83 | 12.28 | 0.54 |
| 10| 2088 | 42.48 | 18.84 | 0.72 |
| 40| 6631 | 99.70 | 54.79 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.46 | 8.69 | 0.47 |
| 2| 770 | 28.55 | 9.40 | 0.48 |
| 3| 953 | 33.47 | 11.46 | 0.55 |
| 5| 1366 | 38.38 | 14.19 | 0.62 |
| 10| 1950 | 43.63 | 18.97 | 0.73 |
| 37| 6096 | 99.28 | 52.64 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 948 | 37.80 | 12.59 | 0.59 |
| 5| 1385 | 44.07 | 15.71 | 0.68 |
| 10| 2039 | 54.17 | 21.84 | 0.84 |
| 29| 4902 | 98.86 | 46.94 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 26.92 | 9.04 | 0.69 |
| 2| 5893 | 34.87 | 11.67 | 0.78 |
| 3| 5968 | 40.40 | 13.47 | 0.84 |
| 4| 6280 | 54.77 | 18.45 | 1.00 |
| 5| 6594 | 67.58 | 22.89 | 1.15 |
| 6| 6672 | 75.80 | 25.69 | 1.24 |
| 7| 6617 | 79.58 | 26.81 | 1.28 |
| 8| 6797 | 88.24 | 29.65 | 1.38 |
| 9| 6861 | 97.07 | 32.56 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.49 | 6.17 | 0.60 |
| 10 | 5 | 284 | 6003 | 29.72 | 10.56 | 0.73 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 39 | 2221 | 7160 | 97.61 | 37.43 | 1.52 |

