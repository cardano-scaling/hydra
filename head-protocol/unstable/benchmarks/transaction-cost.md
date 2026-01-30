--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-30 09:55:07.326532381 UTC |
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
| 1| 5837 | 10.66 | 3.39 | 0.52 |
| 2| 6037 | 12.78 | 4.06 | 0.55 |
| 3| 6236 | 14.47 | 4.57 | 0.57 |
| 5| 6638 | 18.41 | 5.80 | 0.63 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1275 | 6.41 | 3.60 | 0.28 |
| 10| 2183 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 41.40 | 11.97 | 0.60 |
| 4 | 227 | 858 | 50.79 | 14.58 | 0.70 |
| 5 | 283 | 969 | 64.07 | 18.18 | 0.84 |
| 6 | 338 | 1081 | 75.09 | 21.21 | 0.96 |
| 7 | 394 | 1192 | 76.98 | 22.11 | 0.98 |
| 8 | 450 | 1303 | 99.23 | 27.93 | 1.21 |
| 9 | 505 | 1414 | 98.69 | 28.16 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 24.00 | 7.62 | 0.48 |
| 2| 1957 | 25.43 | 8.68 | 0.50 |
| 3| 2057 | 26.94 | 9.77 | 0.53 |
| 5| 2430 | 32.49 | 12.64 | 0.61 |
| 10| 3115 | 41.13 | 18.38 | 0.75 |
| 40| 7371 | 93.49 | 52.94 | 1.61 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.80 | 7.37 | 0.42 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 940 | 26.08 | 9.59 | 0.47 |
| 5| 1221 | 29.22 | 11.81 | 0.53 |
| 10| 2046 | 41.39 | 18.53 | 0.71 |
| 40| 6460 | 96.52 | 53.87 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.51 | 8.47 | 0.46 |
| 2| 886 | 29.86 | 9.81 | 0.50 |
| 3| 903 | 30.23 | 10.54 | 0.51 |
| 5| 1224 | 34.33 | 13.03 | 0.58 |
| 10| 2039 | 44.86 | 19.36 | 0.74 |
| 36| 5803 | 95.03 | 50.73 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 684 | 33.79 | 10.15 | 0.53 |
| 2| 846 | 36.60 | 11.61 | 0.57 |
| 3| 994 | 38.59 | 12.82 | 0.60 |
| 5| 1250 | 42.61 | 15.27 | 0.66 |
| 10| 2017 | 54.14 | 21.83 | 0.83 |
| 28| 4847 | 96.06 | 45.52 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 22.93 | 7.56 | 0.64 |
| 2| 5889 | 32.57 | 10.87 | 0.75 |
| 3| 6230 | 46.81 | 15.84 | 0.92 |
| 4| 6091 | 46.77 | 15.60 | 0.91 |
| 5| 6308 | 58.97 | 19.79 | 1.05 |
| 6| 6505 | 69.62 | 23.39 | 1.17 |
| 7| 6715 | 82.79 | 27.98 | 1.32 |
| 8| 6854 | 93.19 | 31.46 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 284 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 569 | 6173 | 38.81 | 14.21 | 0.84 |
| 10 | 20 | 1141 | 6516 | 58.21 | 21.92 | 1.07 |
| 10 | 39 | 2224 | 7163 | 98.68 | 37.80 | 1.54 |

