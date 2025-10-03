--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-03 10:09:36.875993428 UTC |
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
| 1| 5836 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 13.01 | 4.14 | 0.55 |
| 3| 6238 | 14.88 | 4.72 | 0.58 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7650 | 28.90 | 9.10 | 0.79 |
| 43| 14279 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 915 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 32.20 | 9.36 | 0.51 |
| 3 | 170 | 751 | 41.48 | 11.99 | 0.60 |
| 4 | 228 | 858 | 54.07 | 15.42 | 0.74 |
| 5 | 283 | 969 | 59.43 | 17.10 | 0.80 |
| 6 | 339 | 1081 | 64.40 | 18.66 | 0.85 |
| 7 | 395 | 1192 | 87.14 | 24.58 | 1.08 |
| 8 | 451 | 1303 | 91.91 | 26.08 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1799 | 23.92 | 7.60 | 0.48 |
| 2| 1925 | 25.39 | 8.68 | 0.50 |
| 3| 2082 | 27.31 | 9.86 | 0.53 |
| 5| 2317 | 30.49 | 12.08 | 0.59 |
| 10| 3340 | 43.46 | 19.07 | 0.79 |
| 40| 7700 | 99.93 | 54.74 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 22.57 | 7.31 | 0.41 |
| 2| 741 | 23.54 | 8.22 | 0.43 |
| 3| 955 | 26.95 | 9.86 | 0.48 |
| 5| 1207 | 29.06 | 11.77 | 0.52 |
| 10| 1911 | 37.34 | 17.42 | 0.66 |
| 43| 6742 | 99.26 | 56.62 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 703 | 27.54 | 8.47 | 0.46 |
| 2| 770 | 28.47 | 9.38 | 0.48 |
| 3| 868 | 32.05 | 11.02 | 0.53 |
| 5| 1244 | 36.95 | 13.76 | 0.60 |
| 10| 2121 | 47.89 | 20.19 | 0.78 |
| 34| 5602 | 93.77 | 49.06 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 33.87 | 10.16 | 0.53 |
| 2| 875 | 36.64 | 11.62 | 0.57 |
| 3| 895 | 37.20 | 12.40 | 0.58 |
| 5| 1209 | 41.93 | 15.06 | 0.65 |
| 10| 2039 | 54.29 | 21.86 | 0.84 |
| 29| 4945 | 99.44 | 47.12 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5842 | 27.08 | 9.09 | 0.69 |
| 2| 5895 | 32.45 | 10.86 | 0.75 |
| 3| 5948 | 40.48 | 13.51 | 0.84 |
| 4| 6138 | 50.63 | 16.97 | 0.95 |
| 5| 6408 | 65.09 | 21.99 | 1.12 |
| 6| 6411 | 68.60 | 22.99 | 1.15 |
| 7| 6756 | 81.17 | 27.37 | 1.30 |
| 8| 6775 | 87.94 | 29.53 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 30 | 1706 | 6852 | 81.11 | 30.83 | 1.33 |
| 10 | 38 | 2163 | 7125 | 97.33 | 37.23 | 1.52 |

