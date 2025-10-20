--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-20 13:24:44.538080951 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 19.08 | 6.04 | 0.64 |
| 10| 7644 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.73 | 30.85 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 32.30 | 9.40 | 0.51 |
| 3 | 169 | 747 | 42.65 | 12.27 | 0.62 |
| 4 | 226 | 858 | 53.86 | 15.34 | 0.73 |
| 5 | 283 | 969 | 64.23 | 18.19 | 0.84 |
| 6 | 338 | 1081 | 71.78 | 20.46 | 0.93 |
| 7 | 393 | 1192 | 84.85 | 23.95 | 1.06 |
| 8 | 450 | 1303 | 80.88 | 23.44 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1814 | 24.37 | 7.71 | 0.48 |
| 2| 1883 | 24.80 | 8.49 | 0.49 |
| 3| 2102 | 28.23 | 10.12 | 0.54 |
| 5| 2385 | 30.97 | 12.23 | 0.59 |
| 10| 3188 | 42.11 | 18.66 | 0.77 |
| 44| 7940 | 99.85 | 57.39 | 1.72 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 22.54 | 7.31 | 0.41 |
| 2| 741 | 24.07 | 8.39 | 0.44 |
| 3| 853 | 24.03 | 9.02 | 0.45 |
| 5| 1224 | 29.12 | 11.78 | 0.52 |
| 10| 2009 | 40.69 | 18.36 | 0.70 |
| 42| 6592 | 98.99 | 55.83 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 27.54 | 8.47 | 0.46 |
| 2| 801 | 30.91 | 10.06 | 0.51 |
| 3| 910 | 32.68 | 11.22 | 0.54 |
| 5| 1214 | 37.02 | 13.77 | 0.60 |
| 10| 2009 | 47.35 | 20.02 | 0.77 |
| 37| 6198 | 99.74 | 52.81 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.15 | 0.53 |
| 2| 807 | 35.85 | 11.38 | 0.56 |
| 3| 1074 | 39.26 | 13.03 | 0.61 |
| 5| 1272 | 42.53 | 15.25 | 0.66 |
| 10| 2157 | 55.33 | 22.20 | 0.85 |
| 30| 4941 | 99.30 | 47.71 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 27.08 | 9.09 | 0.69 |
| 2| 5893 | 32.60 | 10.89 | 0.75 |
| 3| 6068 | 42.65 | 14.31 | 0.87 |
| 4| 6316 | 54.73 | 18.47 | 1.00 |
| 5| 6406 | 63.80 | 21.46 | 1.10 |
| 6| 6774 | 77.21 | 26.18 | 1.26 |
| 7| 6767 | 83.85 | 28.34 | 1.33 |
| 8| 6888 | 91.25 | 30.79 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2159 | 7122 | 96.63 | 36.99 | 1.51 |

