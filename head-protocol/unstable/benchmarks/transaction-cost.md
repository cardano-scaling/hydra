--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-20 04:48:20.676571239 UTC |
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
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6640 | 19.26 | 6.10 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.95 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 169 | 751 | 43.91 | 12.57 | 0.63 |
| 4 | 226 | 858 | 52.32 | 14.95 | 0.72 |
| 5 | 282 | 969 | 62.31 | 17.76 | 0.82 |
| 6 | 339 | 1081 | 75.73 | 21.44 | 0.96 |
| 7 | 394 | 1192 | 72.56 | 21.09 | 0.94 |
| 8 | 449 | 1303 | 94.56 | 26.81 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1825 | 24.37 | 7.71 | 0.48 |
| 2| 1951 | 25.76 | 8.76 | 0.51 |
| 3| 2127 | 28.43 | 10.17 | 0.55 |
| 5| 2384 | 30.99 | 12.24 | 0.59 |
| 10| 3312 | 44.04 | 19.21 | 0.79 |
| 42| 7686 | 98.08 | 55.54 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 22.57 | 7.31 | 0.41 |
| 2| 723 | 22.60 | 7.95 | 0.42 |
| 3| 831 | 24.13 | 9.06 | 0.45 |
| 5| 1157 | 27.93 | 11.45 | 0.51 |
| 10| 1942 | 37.36 | 17.42 | 0.66 |
| 40| 6617 | 99.81 | 54.81 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 27.54 | 8.47 | 0.46 |
| 2| 790 | 30.91 | 10.06 | 0.51 |
| 3| 1017 | 31.61 | 10.96 | 0.53 |
| 5| 1230 | 34.29 | 13.02 | 0.58 |
| 10| 2077 | 45.26 | 19.50 | 0.75 |
| 33| 5896 | 97.32 | 49.56 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 33.83 | 10.15 | 0.53 |
| 2| 857 | 36.64 | 11.62 | 0.57 |
| 3| 937 | 37.95 | 12.63 | 0.59 |
| 5| 1230 | 41.82 | 15.03 | 0.65 |
| 10| 2138 | 56.12 | 22.44 | 0.86 |
| 29| 4845 | 97.76 | 46.61 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5787 | 27.09 | 9.08 | 0.69 |
| 2| 5968 | 35.88 | 12.05 | 0.79 |
| 3| 6085 | 44.99 | 15.11 | 0.89 |
| 4| 6096 | 48.98 | 16.38 | 0.93 |
| 5| 6457 | 65.47 | 22.05 | 1.12 |
| 6| 6576 | 73.28 | 24.71 | 1.21 |
| 7| 6791 | 81.00 | 27.34 | 1.30 |
| 8| 6818 | 90.64 | 30.57 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 20 | 1140 | 6514 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1709 | 6855 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2217 | 7157 | 99.31 | 38.01 | 1.54 |

