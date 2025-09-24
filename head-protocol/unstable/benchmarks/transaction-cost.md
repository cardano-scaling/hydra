--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-24 12:46:05.091115605 UTC |
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
| 1| 5836 | 10.57 | 3.36 | 0.52 |
| 2| 6038 | 12.78 | 4.06 | 0.55 |
| 3| 6239 | 14.98 | 4.75 | 0.58 |
| 5| 6640 | 18.88 | 5.97 | 0.64 |
| 10| 7650 | 29.19 | 9.21 | 0.79 |
| 43| 14281 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.17 | 9.59 | 0.52 |
| 3 | 169 | 747 | 42.57 | 12.25 | 0.62 |
| 4 | 225 | 858 | 52.57 | 15.04 | 0.72 |
| 5 | 283 | 969 | 59.15 | 17.03 | 0.79 |
| 6 | 338 | 1085 | 71.93 | 20.50 | 0.93 |
| 7 | 394 | 1192 | 80.26 | 22.84 | 1.02 |
| 8 | 448 | 1303 | 83.00 | 23.89 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1796 | 24.00 | 7.62 | 0.48 |
| 2| 1879 | 24.43 | 8.40 | 0.49 |
| 3| 2141 | 28.38 | 10.16 | 0.55 |
| 5| 2325 | 30.00 | 11.96 | 0.58 |
| 10| 3178 | 40.90 | 18.33 | 0.75 |
| 39| 7543 | 98.09 | 53.58 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 612 | 22.53 | 7.30 | 0.41 |
| 2| 750 | 23.61 | 8.24 | 0.43 |
| 3| 861 | 24.11 | 9.04 | 0.45 |
| 5| 1226 | 29.04 | 11.76 | 0.52 |
| 10| 2005 | 39.83 | 18.14 | 0.69 |
| 42| 6549 | 95.76 | 54.99 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 27.51 | 8.47 | 0.46 |
| 2| 851 | 29.82 | 9.80 | 0.50 |
| 3| 947 | 30.86 | 10.73 | 0.52 |
| 5| 1193 | 36.20 | 13.53 | 0.59 |
| 10| 1945 | 46.84 | 19.85 | 0.76 |
| 35| 5678 | 99.48 | 51.24 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.79 | 10.15 | 0.53 |
| 2| 847 | 36.56 | 11.60 | 0.57 |
| 3| 940 | 37.88 | 12.61 | 0.59 |
| 5| 1303 | 43.35 | 15.49 | 0.67 |
| 10| 2076 | 55.18 | 22.14 | 0.85 |
| 28| 4896 | 97.90 | 46.06 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 27.00 | 9.06 | 0.69 |
| 2| 5891 | 34.80 | 11.64 | 0.77 |
| 3| 6135 | 46.12 | 15.52 | 0.90 |
| 4| 6307 | 55.73 | 18.86 | 1.01 |
| 5| 6548 | 67.88 | 22.95 | 1.15 |
| 6| 6633 | 73.10 | 24.62 | 1.21 |
| 7| 6821 | 81.84 | 27.59 | 1.31 |
| 8| 6921 | 86.78 | 29.27 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.75 | 6.26 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 566 | 6171 | 39.06 | 14.30 | 0.84 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2219 | 7158 | 98.05 | 37.58 | 1.53 |

