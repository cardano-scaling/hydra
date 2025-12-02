--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 04:55:17.695179685 UTC |
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
| 1| 5834 | 10.19 | 3.22 | 0.51 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6242 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.75 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 170 | 747 | 41.36 | 11.94 | 0.60 |
| 4 | 226 | 858 | 49.10 | 14.20 | 0.69 |
| 5 | 283 | 974 | 62.57 | 17.82 | 0.83 |
| 6 | 339 | 1081 | 71.40 | 20.29 | 0.92 |
| 7 | 394 | 1192 | 78.62 | 22.45 | 1.00 |
| 8 | 450 | 1303 | 95.37 | 26.80 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 24.00 | 7.62 | 0.48 |
| 2| 1922 | 25.84 | 8.78 | 0.51 |
| 3| 2059 | 27.35 | 9.87 | 0.53 |
| 5| 2332 | 30.45 | 12.07 | 0.59 |
| 10| 3228 | 42.34 | 18.72 | 0.77 |
| 40| 7517 | 96.33 | 53.73 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 22.57 | 7.33 | 0.41 |
| 2| 786 | 25.16 | 8.70 | 0.45 |
| 3| 876 | 25.09 | 9.33 | 0.46 |
| 5| 1157 | 27.97 | 11.46 | 0.51 |
| 10| 2113 | 42.12 | 18.76 | 0.72 |
| 42| 6577 | 97.16 | 55.37 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 27.50 | 8.46 | 0.46 |
| 2| 779 | 30.87 | 10.05 | 0.51 |
| 3| 960 | 30.94 | 10.75 | 0.52 |
| 5| 1249 | 37.02 | 13.78 | 0.60 |
| 10| 2013 | 48.15 | 20.25 | 0.77 |
| 37| 5946 | 97.46 | 52.07 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 662 | 33.87 | 10.16 | 0.53 |
| 2| 761 | 35.21 | 11.18 | 0.55 |
| 3| 1064 | 39.27 | 13.03 | 0.61 |
| 5| 1263 | 42.49 | 15.24 | 0.66 |
| 10| 1922 | 52.60 | 21.37 | 0.81 |
| 30| 4838 | 98.36 | 47.42 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5844 | 27.00 | 9.07 | 0.69 |
| 2| 6000 | 36.73 | 12.41 | 0.80 |
| 3| 6065 | 42.21 | 14.17 | 0.86 |
| 4| 6395 | 55.89 | 18.88 | 1.02 |
| 5| 6446 | 66.17 | 22.33 | 1.13 |
| 6| 6497 | 70.65 | 23.74 | 1.18 |
| 7| 6605 | 76.83 | 25.81 | 1.25 |
| 8| 6959 | 89.15 | 30.02 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 20.07 | 6.71 | 0.62 |
| 10 | 1 | 57 | 5868 | 22.10 | 7.52 | 0.64 |
| 10 | 5 | 285 | 6004 | 29.79 | 10.58 | 0.73 |
| 10 | 10 | 569 | 6174 | 38.62 | 14.15 | 0.84 |
| 10 | 39 | 2217 | 7157 | 98.93 | 37.88 | 1.54 |

