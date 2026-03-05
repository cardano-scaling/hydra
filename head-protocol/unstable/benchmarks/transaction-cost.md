--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-05 13:12:52.261883633 UTC |
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
| 1| 5834 | 10.78 | 3.43 | 0.52 |
| 2| 6037 | 12.32 | 3.89 | 0.54 |
| 3| 6238 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 18.62 | 5.87 | 0.64 |
| 10| 7651 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 916 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 524 | 18.25 | 5.73 | 0.36 |
| 2 | 113 | 636 | 24.62 | 7.68 | 0.43 |
| 3 | 170 | 747 | 31.29 | 9.72 | 0.51 |
| 4 | 226 | 858 | 37.16 | 11.55 | 0.57 |
| 5 | 284 | 974 | 46.38 | 14.22 | 0.67 |
| 6 | 338 | 1081 | 53.99 | 16.52 | 0.75 |
| 7 | 394 | 1196 | 63.07 | 19.17 | 0.85 |
| 8 | 451 | 1307 | 71.47 | 21.68 | 0.94 |
| 9 | 506 | 1414 | 70.99 | 22.01 | 0.94 |
| 10 | 560 | 1525 | 95.47 | 28.54 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1804 | 18.10 | 6.30 | 0.42 |
| 2| 1924 | 18.93 | 7.23 | 0.44 |
| 3| 2013 | 19.41 | 8.03 | 0.45 |
| 5| 2411 | 23.85 | 10.70 | 0.53 |
| 10| 3328 | 32.56 | 16.64 | 0.68 |
| 50| 9021 | 85.74 | 59.05 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 16.77 | 6.01 | 0.36 |
| 2| 787 | 17.70 | 6.97 | 0.38 |
| 3| 898 | 19.01 | 8.02 | 0.40 |
| 5| 1115 | 20.02 | 9.62 | 0.43 |
| 10| 2156 | 30.77 | 16.20 | 0.61 |
| 50| 8026 | 88.60 | 59.93 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 20.72 | 6.94 | 0.40 |
| 2| 827 | 22.02 | 8.00 | 0.42 |
| 3| 915 | 24.69 | 9.43 | 0.46 |
| 5| 1348 | 28.92 | 12.07 | 0.53 |
| 10| 1960 | 33.46 | 16.75 | 0.63 |
| 49| 7879 | 94.75 | 61.00 | 1.70 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 683 | 25.34 | 8.25 | 0.44 |
| 2| 764 | 26.31 | 9.19 | 0.46 |
| 3| 997 | 29.01 | 10.68 | 0.51 |
| 5| 1241 | 31.97 | 12.88 | 0.56 |
| 10| 2053 | 41.44 | 19.04 | 0.71 |
| 44| 6938 | 99.93 | 58.84 | 1.69 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 23.77 | 8.34 | 0.66 |
| 2| 5936 | 31.31 | 11.02 | 0.74 |
| 3| 6036 | 38.97 | 13.76 | 0.83 |
| 4| 6261 | 47.87 | 16.93 | 0.93 |
| 5| 6462 | 56.21 | 19.95 | 1.03 |
| 6| 6723 | 66.44 | 23.63 | 1.15 |
| 7| 6647 | 73.92 | 26.11 | 1.23 |
| 8| 6867 | 78.70 | 27.88 | 1.29 |
| 9| 6944 | 87.07 | 30.74 | 1.38 |
| 10| 7306 | 94.87 | 33.46 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.24 | 5.97 | 0.59 |
| 10 | 1 | 57 | 5868 | 18.63 | 6.58 | 0.61 |
| 10 | 5 | 284 | 6004 | 26.27 | 9.74 | 0.70 |
| 10 | 10 | 570 | 6175 | 35.88 | 13.71 | 0.81 |
| 10 | 20 | 1139 | 6513 | 52.68 | 20.79 | 1.01 |
| 10 | 40 | 2279 | 7196 | 89.98 | 36.23 | 1.46 |
| 10 | 45 | 2562 | 7364 | 99.93 | 40.31 | 1.57 |

