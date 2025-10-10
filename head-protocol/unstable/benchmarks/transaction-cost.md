--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-10 09:28:30.079367599 UTC |
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
| 1| 5837 | 10.55 | 3.35 | 0.52 |
| 2| 6037 | 12.70 | 4.03 | 0.55 |
| 3| 6240 | 14.47 | 4.57 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10077 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 170 | 747 | 40.24 | 11.71 | 0.59 |
| 4 | 225 | 858 | 48.27 | 14.03 | 0.68 |
| 5 | 283 | 969 | 63.24 | 18.02 | 0.83 |
| 6 | 340 | 1081 | 70.01 | 20.07 | 0.91 |
| 7 | 394 | 1192 | 73.11 | 21.22 | 0.95 |
| 8 | 451 | 1303 | 94.94 | 26.91 | 1.17 |
| 9 | 506 | 1414 | 96.66 | 27.67 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.37 | 7.71 | 0.48 |
| 2| 1962 | 26.54 | 9.00 | 0.52 |
| 3| 2066 | 26.95 | 9.77 | 0.53 |
| 5| 2370 | 31.12 | 12.27 | 0.59 |
| 10| 3143 | 40.55 | 18.24 | 0.75 |
| 39| 7377 | 93.38 | 52.26 | 1.61 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 652 | 22.54 | 7.31 | 0.41 |
| 2| 809 | 25.36 | 8.75 | 0.45 |
| 3| 900 | 25.14 | 9.33 | 0.46 |
| 5| 1266 | 32.46 | 12.71 | 0.56 |
| 10| 1889 | 38.25 | 17.68 | 0.67 |
| 40| 6479 | 97.58 | 54.15 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 29.17 | 8.91 | 0.48 |
| 2| 778 | 30.94 | 10.07 | 0.51 |
| 3| 941 | 32.72 | 11.23 | 0.54 |
| 5| 1205 | 36.39 | 13.58 | 0.60 |
| 10| 2160 | 46.51 | 19.86 | 0.76 |
| 37| 6138 | 99.95 | 52.84 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 672 | 33.83 | 10.15 | 0.53 |
| 2| 806 | 35.85 | 11.38 | 0.56 |
| 3| 983 | 38.51 | 12.80 | 0.60 |
| 5| 1255 | 42.45 | 15.23 | 0.66 |
| 10| 1954 | 53.46 | 21.62 | 0.82 |
| 29| 4749 | 97.06 | 46.42 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.08 | 9.08 | 0.69 |
| 2| 5974 | 35.80 | 12.02 | 0.79 |
| 3| 6098 | 46.08 | 15.54 | 0.90 |
| 4| 6118 | 49.39 | 16.52 | 0.94 |
| 5| 6355 | 64.19 | 21.64 | 1.10 |
| 6| 6773 | 75.98 | 25.90 | 1.25 |
| 7| 6682 | 81.95 | 27.51 | 1.30 |
| 8| 6800 | 86.99 | 29.33 | 1.36 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.63 | 6.56 | 0.61 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1138 | 6512 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.68 | 37.80 | 1.54 |

