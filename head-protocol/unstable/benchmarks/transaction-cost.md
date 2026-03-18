--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-18 13:08:13.776782325 UTC |
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
| 1| 5836 | 10.48 | 3.33 | 0.52 |
| 2| 6041 | 12.63 | 4.00 | 0.55 |
| 3| 6238 | 14.40 | 4.55 | 0.57 |
| 5| 6645 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 924 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2175 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.71 | 5.86 | 0.37 |
| 2 | 114 | 636 | 24.74 | 7.72 | 0.43 |
| 3 | 170 | 747 | 32.02 | 9.89 | 0.51 |
| 4 | 226 | 858 | 35.78 | 11.20 | 0.56 |
| 5 | 281 | 969 | 45.98 | 14.12 | 0.67 |
| 6 | 337 | 1081 | 49.85 | 15.45 | 0.71 |
| 7 | 393 | 1192 | 60.50 | 18.60 | 0.83 |
| 8 | 449 | 1303 | 67.21 | 20.62 | 0.90 |
| 9 | 504 | 1414 | 91.02 | 27.16 | 1.14 |
| 10 | 560 | 1525 | 86.04 | 26.16 | 1.10 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 17.84 | 6.24 | 0.42 |
| 2| 1882 | 18.27 | 7.02 | 0.43 |
| 3| 2148 | 21.04 | 8.52 | 0.48 |
| 5| 2388 | 23.07 | 10.46 | 0.52 |
| 10| 3039 | 28.67 | 15.47 | 0.63 |
| 50| 9223 | 87.06 | 59.50 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 612 | 16.65 | 5.99 | 0.35 |
| 2| 768 | 17.49 | 6.87 | 0.37 |
| 3| 1009 | 20.72 | 8.53 | 0.42 |
| 5| 1254 | 21.85 | 10.18 | 0.46 |
| 10| 2010 | 29.10 | 15.71 | 0.59 |
| 50| 7924 | 85.13 | 58.99 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 20.74 | 6.95 | 0.40 |
| 2| 862 | 23.88 | 8.54 | 0.44 |
| 3| 868 | 24.13 | 9.25 | 0.45 |
| 5| 1248 | 28.01 | 11.76 | 0.52 |
| 10| 1904 | 35.11 | 17.20 | 0.64 |
| 49| 7952 | 95.96 | 61.38 | 1.72 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 25.37 | 8.26 | 0.44 |
| 2| 810 | 26.92 | 9.38 | 0.47 |
| 3| 946 | 28.37 | 10.47 | 0.50 |
| 5| 1204 | 31.51 | 12.73 | 0.55 |
| 10| 2012 | 40.92 | 18.87 | 0.71 |
| 42| 6933 | 99.82 | 57.56 | 1.68 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5808 | 23.78 | 8.35 | 0.66 |
| 2| 5955 | 31.33 | 11.06 | 0.75 |
| 3| 6123 | 39.58 | 14.01 | 0.84 |
| 4| 6267 | 47.51 | 16.82 | 0.93 |
| 5| 6412 | 53.00 | 18.79 | 1.00 |
| 6| 6573 | 63.11 | 22.36 | 1.11 |
| 7| 6665 | 71.32 | 25.31 | 1.20 |
| 8| 6830 | 80.74 | 28.44 | 1.31 |
| 9| 7238 | 90.78 | 32.41 | 1.44 |
| 10| 7110 | 92.71 | 32.71 | 1.45 |
| 11| 7097 | 97.99 | 34.74 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.24 | 5.97 | 0.59 |
| 10 | 1 | 57 | 5869 | 18.22 | 6.44 | 0.60 |
| 10 | 5 | 285 | 6004 | 25.03 | 9.31 | 0.68 |
| 10 | 10 | 568 | 6172 | 34.47 | 13.22 | 0.80 |
| 10 | 20 | 1139 | 6513 | 53.68 | 21.14 | 1.02 |
| 10 | 30 | 1708 | 6854 | 71.33 | 28.51 | 1.23 |
| 10 | 40 | 2278 | 7194 | 89.57 | 36.08 | 1.45 |
| 10 | 45 | 2564 | 7365 | 99.10 | 40.02 | 1.56 |

