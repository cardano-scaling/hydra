--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 21:24:10.191245635 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6038 | 12.44 | 3.94 | 0.54 |
| 3| 6238 | 15.05 | 4.78 | 0.58 |
| 5| 6641 | 18.52 | 5.84 | 0.63 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 556 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 636 | 33.33 | 9.64 | 0.52 |
| 3 | 171 | 747 | 43.78 | 12.57 | 0.63 |
| 4 | 227 | 858 | 49.54 | 14.31 | 0.69 |
| 5 | 284 | 969 | 59.52 | 17.06 | 0.80 |
| 6 | 339 | 1081 | 73.24 | 20.73 | 0.94 |
| 7 | 395 | 1192 | 82.82 | 23.54 | 1.04 |
| 8 | 450 | 1303 | 98.46 | 27.65 | 1.20 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1800 | 23.92 | 7.60 | 0.48 |
| 2| 1883 | 24.47 | 8.41 | 0.49 |
| 3| 2013 | 26.31 | 9.58 | 0.52 |
| 5| 2335 | 30.04 | 11.97 | 0.58 |
| 10| 3281 | 44.14 | 19.23 | 0.79 |
| 39| 7472 | 96.64 | 53.14 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 616 | 22.84 | 7.39 | 0.42 |
| 2| 739 | 24.00 | 8.38 | 0.44 |
| 3| 853 | 24.03 | 9.02 | 0.45 |
| 5| 1229 | 29.86 | 12.00 | 0.53 |
| 10| 1959 | 38.51 | 17.74 | 0.68 |
| 40| 6406 | 97.70 | 54.12 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 26.83 | 8.26 | 0.45 |
| 2| 885 | 29.86 | 9.81 | 0.50 |
| 3| 1041 | 34.11 | 11.65 | 0.56 |
| 5| 1387 | 36.31 | 13.64 | 0.60 |
| 10| 2104 | 48.83 | 20.46 | 0.78 |
| 37| 6123 | 98.19 | 52.35 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 33.87 | 10.16 | 0.53 |
| 2| 875 | 36.52 | 11.59 | 0.57 |
| 3| 1009 | 38.55 | 12.81 | 0.60 |
| 5| 1267 | 42.49 | 15.24 | 0.66 |
| 10| 2131 | 55.26 | 22.16 | 0.85 |
| 28| 4667 | 95.48 | 45.30 | 1.45 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5806 | 27.05 | 9.07 | 0.69 |
| 2| 5995 | 36.93 | 12.45 | 0.80 |
| 3| 6014 | 41.59 | 13.94 | 0.85 |
| 4| 6166 | 52.90 | 17.73 | 0.98 |
| 5| 6559 | 65.61 | 22.18 | 1.13 |
| 6| 6627 | 74.49 | 25.19 | 1.23 |
| 7| 6818 | 85.80 | 29.03 | 1.35 |
| 8| 6963 | 94.26 | 31.75 | 1.45 |
| 9| 6931 | 96.79 | 32.54 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 22.10 | 7.52 | 0.64 |
| 10 | 5 | 285 | 6004 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 570 | 6174 | 39.95 | 14.60 | 0.85 |
| 10 | 30 | 1710 | 6856 | 80.67 | 30.67 | 1.32 |
| 10 | 38 | 2164 | 7126 | 97.33 | 37.23 | 1.52 |

