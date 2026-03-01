--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-01 11:29:33.335443713 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | 5788da8969b01bb1d9fd7b78b0dcd988ef2b1d4519e0deae656cef53 | 12374 | 
| μHead | d81fa4e721cac05546c901514e27fad626a1f6a8e11b4d6113d85dee* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5837 | 10.61 | 3.37 | 0.52 |
| 2| 6038 | 12.99 | 4.13 | 0.55 |
| 3| 6238 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.50 | 5.83 | 0.63 |
| 10| 7646 | 29.31 | 9.25 | 0.79 |
| 43| 14279 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2181 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 18.71 | 5.86 | 0.37 |
| 2 | 114 | 636 | 24.15 | 7.57 | 0.43 |
| 3 | 168 | 747 | 30.91 | 9.64 | 0.50 |
| 4 | 225 | 862 | 36.27 | 11.35 | 0.56 |
| 5 | 285 | 969 | 47.00 | 14.41 | 0.68 |
| 6 | 337 | 1081 | 52.38 | 16.08 | 0.74 |
| 7 | 394 | 1192 | 59.34 | 18.14 | 0.81 |
| 8 | 449 | 1303 | 71.45 | 21.68 | 0.94 |
| 9 | 505 | 1414 | 77.85 | 23.61 | 1.01 |
| 10 | 560 | 1529 | 97.29 | 29.02 | 1.21 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1802 | 18.05 | 6.29 | 0.42 |
| 2| 1882 | 18.45 | 7.07 | 0.43 |
| 3| 2116 | 20.91 | 8.48 | 0.47 |
| 5| 2507 | 24.82 | 10.98 | 0.54 |
| 10| 3256 | 31.66 | 16.37 | 0.67 |
| 50| 9610 | 92.78 | 61.21 | 1.76 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 638 | 16.65 | 6.00 | 0.36 |
| 2| 771 | 17.72 | 6.98 | 0.38 |
| 3| 925 | 18.58 | 7.86 | 0.40 |
| 5| 1251 | 22.66 | 10.45 | 0.46 |
| 10| 1958 | 28.35 | 15.46 | 0.58 |
| 50| 8107 | 85.94 | 59.25 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 677 | 20.72 | 6.94 | 0.40 |
| 2| 832 | 21.99 | 7.99 | 0.42 |
| 3| 1053 | 24.33 | 9.38 | 0.46 |
| 5| 1285 | 28.40 | 11.90 | 0.52 |
| 10| 1932 | 35.42 | 17.29 | 0.65 |
| 50| 8214 | 98.71 | 62.88 | 1.76 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 691 | 25.37 | 8.26 | 0.45 |
| 2| 765 | 26.33 | 9.19 | 0.46 |
| 3| 938 | 28.44 | 10.50 | 0.50 |
| 5| 1250 | 31.71 | 12.80 | 0.55 |
| 10| 1912 | 39.77 | 18.51 | 0.69 |
| 41| 6579 | 95.97 | 55.74 | 1.62 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 23.74 | 8.33 | 0.66 |
| 2| 5820 | 27.59 | 9.60 | 0.70 |
| 3| 6042 | 38.18 | 13.46 | 0.82 |
| 4| 6285 | 45.53 | 16.14 | 0.91 |
| 5| 6516 | 57.53 | 20.47 | 1.05 |
| 6| 6555 | 61.06 | 21.62 | 1.09 |
| 7| 6707 | 72.88 | 25.73 | 1.22 |
| 8| 6786 | 78.44 | 27.67 | 1.28 |
| 9| 6943 | 83.01 | 29.23 | 1.34 |
| 10| 7042 | 96.22 | 33.79 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.83 | 5.83 | 0.58 |
| 10 | 1 | 57 | 5869 | 18.63 | 6.58 | 0.61 |
| 10 | 5 | 285 | 6004 | 26.27 | 9.74 | 0.70 |
| 10 | 10 | 569 | 6174 | 35.71 | 13.65 | 0.81 |
| 10 | 20 | 1138 | 6512 | 53.51 | 21.08 | 1.02 |
| 10 | 30 | 1708 | 6855 | 72.57 | 28.94 | 1.25 |
| 10 | 43 | 2447 | 7294 | 95.87 | 38.65 | 1.52 |

