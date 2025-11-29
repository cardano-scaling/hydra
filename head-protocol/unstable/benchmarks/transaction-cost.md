--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-29 10:13:33.487361716 UTC |
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
| 1| 5837 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.46 | 3.94 | 0.55 |
| 3| 6236 | 14.60 | 4.62 | 0.58 |
| 5| 6638 | 18.79 | 5.94 | 0.64 |
| 10| 7647 | 29.28 | 9.24 | 0.79 |
| 43| 14281 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1272 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 170 | 747 | 43.91 | 12.57 | 0.63 |
| 4 | 226 | 858 | 52.46 | 15.01 | 0.72 |
| 5 | 284 | 969 | 60.87 | 17.41 | 0.81 |
| 6 | 340 | 1081 | 73.57 | 20.85 | 0.94 |
| 7 | 395 | 1196 | 74.81 | 21.59 | 0.96 |
| 8 | 450 | 1303 | 87.88 | 25.17 | 1.10 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.37 | 7.71 | 0.48 |
| 2| 1955 | 26.30 | 8.92 | 0.51 |
| 3| 2154 | 28.06 | 10.09 | 0.54 |
| 5| 2359 | 31.03 | 12.25 | 0.59 |
| 10| 3162 | 40.76 | 18.29 | 0.75 |
| 38| 7547 | 96.34 | 52.45 | 1.64 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 22.80 | 7.36 | 0.41 |
| 2| 722 | 22.56 | 7.94 | 0.42 |
| 3| 919 | 25.83 | 9.55 | 0.47 |
| 5| 1257 | 30.60 | 12.21 | 0.54 |
| 10| 1968 | 37.79 | 17.54 | 0.67 |
| 40| 6577 | 99.23 | 54.67 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 27.51 | 8.47 | 0.46 |
| 2| 800 | 29.22 | 9.61 | 0.49 |
| 3| 911 | 32.64 | 11.21 | 0.53 |
| 5| 1207 | 34.33 | 13.03 | 0.57 |
| 10| 1930 | 46.55 | 19.78 | 0.75 |
| 37| 6184 | 99.40 | 52.70 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.16 | 0.53 |
| 2| 854 | 36.56 | 11.60 | 0.57 |
| 3| 1062 | 39.26 | 13.03 | 0.61 |
| 5| 1329 | 42.93 | 15.37 | 0.67 |
| 10| 2044 | 54.69 | 22.00 | 0.84 |
| 30| 4910 | 99.43 | 47.73 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 26.92 | 9.04 | 0.69 |
| 2| 5997 | 36.81 | 12.43 | 0.80 |
| 3| 6111 | 44.80 | 15.08 | 0.89 |
| 4| 6207 | 53.93 | 18.10 | 0.99 |
| 5| 6504 | 66.36 | 22.41 | 1.13 |
| 6| 6497 | 66.83 | 22.44 | 1.14 |
| 7| 6468 | 72.97 | 24.41 | 1.20 |
| 8| 6944 | 93.73 | 31.65 | 1.44 |
| 9| 6885 | 95.77 | 32.17 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 569 | 6174 | 39.25 | 14.36 | 0.84 |
| 10 | 20 | 1141 | 6515 | 59.73 | 22.44 | 1.08 |
| 10 | 30 | 1705 | 6852 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2222 | 7161 | 98.86 | 37.86 | 1.54 |

