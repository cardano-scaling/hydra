--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-08 12:21:52.052186518 UTC |
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
| 1| 5834 | 10.66 | 3.39 | 0.52 |
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6242 | 14.59 | 4.61 | 0.58 |
| 5| 6640 | 19.19 | 6.08 | 0.64 |
| 10| 7646 | 29.28 | 9.24 | 0.79 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 924 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10041 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 114 | 640 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 40.14 | 11.65 | 0.59 |
| 4 | 226 | 858 | 47.63 | 13.85 | 0.67 |
| 5 | 283 | 969 | 62.35 | 17.74 | 0.82 |
| 6 | 340 | 1081 | 66.86 | 19.32 | 0.88 |
| 7 | 393 | 1196 | 71.79 | 20.85 | 0.93 |
| 8 | 450 | 1303 | 96.40 | 27.20 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.37 | 7.71 | 0.48 |
| 2| 1916 | 25.85 | 8.78 | 0.51 |
| 3| 2078 | 26.94 | 9.77 | 0.53 |
| 5| 2425 | 32.49 | 12.64 | 0.61 |
| 10| 3202 | 42.18 | 18.68 | 0.77 |
| 42| 7782 | 99.21 | 55.86 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 22.50 | 7.30 | 0.41 |
| 2| 763 | 24.01 | 8.38 | 0.44 |
| 3| 857 | 24.11 | 9.04 | 0.45 |
| 5| 1157 | 28.12 | 11.50 | 0.51 |
| 10| 1985 | 38.41 | 17.73 | 0.68 |
| 41| 6633 | 96.39 | 54.51 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 659 | 29.09 | 8.89 | 0.48 |
| 2| 737 | 30.23 | 9.85 | 0.50 |
| 3| 1069 | 34.15 | 11.66 | 0.56 |
| 5| 1257 | 35.01 | 13.24 | 0.58 |
| 10| 1983 | 47.55 | 20.07 | 0.77 |
| 35| 5738 | 98.62 | 51.04 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.79 | 10.15 | 0.53 |
| 2| 812 | 35.85 | 11.38 | 0.56 |
| 3| 900 | 37.20 | 12.40 | 0.58 |
| 5| 1200 | 41.97 | 15.07 | 0.65 |
| 10| 2012 | 53.91 | 21.77 | 0.83 |
| 29| 4749 | 95.73 | 46.03 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5833 | 27.08 | 9.09 | 0.69 |
| 2| 5823 | 31.45 | 10.46 | 0.74 |
| 3| 5993 | 40.20 | 13.41 | 0.84 |
| 4| 6436 | 57.11 | 19.33 | 1.03 |
| 5| 6497 | 65.33 | 22.00 | 1.12 |
| 6| 6507 | 70.56 | 23.79 | 1.18 |
| 7| 6580 | 78.09 | 26.18 | 1.26 |
| 8| 6815 | 93.59 | 31.50 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.66 | 7.37 | 0.64 |
| 10 | 10 | 569 | 6173 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1140 | 6515 | 59.54 | 22.38 | 1.08 |
| 10 | 30 | 1710 | 6857 | 79.78 | 30.37 | 1.32 |
| 10 | 39 | 2219 | 7159 | 99.82 | 38.19 | 1.55 |

