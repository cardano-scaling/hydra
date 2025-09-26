--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-26 11:38:25.409581873 UTC |
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
| 1| 5840 | 10.69 | 3.40 | 0.52 |
| 2| 6037 | 12.34 | 3.90 | 0.54 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.91 | 5.98 | 0.64 |
| 10| 7647 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 99.11 | 30.98 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10068 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 42.62 | 12.24 | 0.62 |
| 4 | 225 | 858 | 51.97 | 14.89 | 0.72 |
| 5 | 281 | 969 | 55.84 | 16.21 | 0.76 |
| 6 | 339 | 1081 | 67.50 | 19.39 | 0.88 |
| 7 | 393 | 1192 | 86.63 | 24.41 | 1.08 |
| 8 | 450 | 1307 | 90.76 | 25.75 | 1.13 |
| 9 | 506 | 1414 | 94.32 | 27.06 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.37 | 7.71 | 0.48 |
| 2| 1991 | 26.58 | 9.01 | 0.52 |
| 3| 2064 | 26.94 | 9.77 | 0.53 |
| 5| 2529 | 33.32 | 12.90 | 0.62 |
| 10| 3105 | 40.00 | 18.07 | 0.74 |
| 40| 7574 | 98.39 | 54.30 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 22.81 | 7.37 | 0.42 |
| 2| 778 | 24.28 | 8.45 | 0.44 |
| 3| 838 | 24.09 | 9.04 | 0.45 |
| 5| 1306 | 31.21 | 12.39 | 0.55 |
| 10| 2019 | 39.62 | 18.05 | 0.69 |
| 42| 6532 | 95.95 | 55.05 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 605 | 28.46 | 8.69 | 0.47 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 914 | 32.76 | 11.24 | 0.54 |
| 5| 1274 | 35.04 | 13.25 | 0.58 |
| 10| 2081 | 47.69 | 20.12 | 0.77 |
| 34| 5532 | 96.21 | 49.67 | 1.53 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 33.87 | 10.16 | 0.53 |
| 2| 907 | 36.60 | 11.61 | 0.57 |
| 3| 1014 | 38.55 | 12.81 | 0.60 |
| 5| 1309 | 43.21 | 15.46 | 0.67 |
| 10| 2012 | 54.14 | 21.83 | 0.83 |
| 30| 4910 | 98.40 | 47.45 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5816 | 27.00 | 9.07 | 0.69 |
| 2| 5905 | 32.37 | 10.82 | 0.75 |
| 3| 6067 | 44.72 | 15.06 | 0.89 |
| 4| 6160 | 50.40 | 16.87 | 0.95 |
| 5| 6438 | 61.39 | 20.66 | 1.08 |
| 6| 6481 | 69.67 | 23.36 | 1.17 |
| 7| 6804 | 83.46 | 28.11 | 1.33 |
| 8| 6818 | 88.78 | 29.85 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1134 | 6508 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 40 | 2276 | 7192 | 98.77 | 37.94 | 1.54 |

