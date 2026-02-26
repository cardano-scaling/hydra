--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-26 10:09:38.812338607 UTC |
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
| 1| 5834 | 10.40 | 3.30 | 0.51 |
| 2| 6035 | 13.01 | 4.14 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 29.11 | 9.17 | 0.79 |
| 43| 14282 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.25 | 5.74 | 0.36 |
| 2 | 113 | 636 | 25.24 | 7.85 | 0.44 |
| 3 | 171 | 747 | 30.27 | 9.48 | 0.50 |
| 4 | 225 | 858 | 38.36 | 11.85 | 0.58 |
| 5 | 283 | 969 | 43.64 | 13.56 | 0.64 |
| 6 | 340 | 1081 | 53.69 | 16.38 | 0.75 |
| 7 | 392 | 1192 | 63.64 | 19.32 | 0.86 |
| 8 | 448 | 1303 | 70.31 | 21.40 | 0.93 |
| 9 | 507 | 1414 | 79.78 | 24.06 | 1.03 |
| 10 | 560 | 1525 | 74.28 | 23.17 | 0.98 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1820 | 18.10 | 6.30 | 0.42 |
| 2| 1882 | 18.45 | 7.07 | 0.43 |
| 3| 2097 | 20.79 | 8.45 | 0.47 |
| 5| 2358 | 23.17 | 10.49 | 0.52 |
| 10| 3128 | 30.32 | 15.96 | 0.65 |
| 50| 9303 | 88.72 | 60.00 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 16.64 | 5.98 | 0.36 |
| 2| 722 | 16.72 | 6.64 | 0.37 |
| 3| 943 | 19.87 | 8.27 | 0.41 |
| 5| 1139 | 20.75 | 9.85 | 0.44 |
| 10| 2116 | 30.48 | 16.12 | 0.61 |
| 50| 8287 | 91.85 | 60.94 | 1.70 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 20.72 | 6.94 | 0.40 |
| 2| 888 | 22.56 | 8.17 | 0.43 |
| 3| 960 | 25.20 | 9.60 | 0.47 |
| 5| 1316 | 26.95 | 11.49 | 0.51 |
| 10| 2039 | 36.48 | 17.64 | 0.66 |
| 50| 8302 | 98.35 | 62.83 | 1.76 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 25.34 | 8.25 | 0.44 |
| 2| 807 | 26.85 | 9.36 | 0.47 |
| 3| 896 | 27.85 | 10.30 | 0.49 |
| 5| 1246 | 32.10 | 12.92 | 0.56 |
| 10| 2036 | 40.75 | 18.82 | 0.70 |
| 42| 6713 | 97.08 | 56.72 | 1.64 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5809 | 23.71 | 8.32 | 0.66 |
| 2| 5891 | 30.38 | 10.66 | 0.73 |
| 3| 6174 | 39.82 | 14.14 | 0.85 |
| 4| 6155 | 43.51 | 15.33 | 0.88 |
| 5| 6338 | 53.62 | 18.94 | 1.00 |
| 6| 6592 | 64.99 | 23.08 | 1.13 |
| 7| 6696 | 69.93 | 24.84 | 1.19 |
| 8| 6911 | 77.31 | 27.53 | 1.28 |
| 9| 6969 | 82.23 | 29.11 | 1.33 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.41 | 6.04 | 0.59 |
| 10 | 1 | 57 | 5868 | 18.63 | 6.58 | 0.61 |
| 10 | 10 | 570 | 6175 | 35.29 | 13.51 | 0.80 |
| 10 | 20 | 1137 | 6512 | 54.75 | 21.51 | 1.03 |
| 10 | 30 | 1707 | 6853 | 71.74 | 28.65 | 1.24 |
| 10 | 40 | 2275 | 7192 | 91.22 | 36.66 | 1.47 |
| 10 | 45 | 2559 | 7360 | 99.10 | 40.02 | 1.56 |

