--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-27 14:13:55.5851835 UTC |
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
| 1| 5837 | 10.26 | 3.25 | 0.51 |
| 2| 6038 | 12.46 | 3.94 | 0.55 |
| 3| 6242 | 14.52 | 4.59 | 0.58 |
| 5| 6643 | 18.81 | 5.94 | 0.64 |
| 10| 7646 | 29.14 | 9.19 | 0.79 |
| 43| 14279 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 114 | 640 | 24.67 | 7.69 | 0.43 |
| 3 | 170 | 751 | 31.51 | 9.81 | 0.51 |
| 4 | 227 | 858 | 39.34 | 12.10 | 0.59 |
| 5 | 284 | 969 | 44.93 | 13.86 | 0.66 |
| 6 | 338 | 1081 | 54.40 | 16.60 | 0.76 |
| 7 | 394 | 1192 | 63.98 | 19.45 | 0.86 |
| 8 | 451 | 1303 | 66.98 | 20.56 | 0.90 |
| 9 | 505 | 1414 | 82.04 | 24.72 | 1.05 |
| 10 | 560 | 1525 | 77.39 | 23.89 | 1.01 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1804 | 18.10 | 6.30 | 0.42 |
| 2| 1886 | 18.43 | 7.06 | 0.43 |
| 3| 2150 | 21.04 | 8.52 | 0.48 |
| 5| 2359 | 23.28 | 10.52 | 0.52 |
| 10| 3127 | 30.18 | 15.92 | 0.65 |
| 50| 9450 | 90.80 | 60.62 | 1.74 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 16.64 | 5.98 | 0.36 |
| 2| 759 | 17.74 | 7.00 | 0.38 |
| 3| 933 | 18.84 | 7.98 | 0.40 |
| 5| 1330 | 23.61 | 10.73 | 0.48 |
| 10| 2032 | 29.48 | 15.81 | 0.59 |
| 50| 7946 | 86.11 | 59.28 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 705 | 20.72 | 6.94 | 0.40 |
| 2| 854 | 22.53 | 8.17 | 0.43 |
| 3| 1020 | 23.81 | 9.21 | 0.45 |
| 5| 1188 | 27.32 | 11.54 | 0.51 |
| 10| 2033 | 34.05 | 16.94 | 0.64 |
| 50| 7903 | 93.63 | 61.29 | 1.70 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 25.37 | 8.26 | 0.44 |
| 2| 834 | 26.87 | 9.37 | 0.47 |
| 3| 899 | 27.82 | 10.30 | 0.49 |
| 5| 1243 | 31.76 | 12.81 | 0.55 |
| 10| 2014 | 40.77 | 18.83 | 0.70 |
| 42| 6818 | 98.46 | 57.12 | 1.66 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5793 | 23.78 | 8.34 | 0.66 |
| 2| 5871 | 30.37 | 10.67 | 0.73 |
| 3| 6070 | 39.15 | 13.86 | 0.83 |
| 4| 6232 | 44.59 | 15.77 | 0.90 |
| 5| 6430 | 56.06 | 19.91 | 1.03 |
| 6| 6483 | 61.39 | 21.65 | 1.09 |
| 7| 6730 | 70.55 | 25.04 | 1.20 |
| 8| 6977 | 84.32 | 29.93 | 1.35 |
| 9| 7009 | 86.30 | 30.53 | 1.38 |
| 11| 7032 | 99.98 | 35.18 | 1.52 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 16.83 | 5.83 | 0.59 |
| 10 | 1 | 57 | 5869 | 18.63 | 6.58 | 0.61 |
| 10 | 5 | 285 | 6004 | 25.85 | 9.60 | 0.69 |
| 10 | 20 | 1141 | 6515 | 54.09 | 21.28 | 1.03 |
| 10 | 30 | 1705 | 6852 | 71.50 | 28.57 | 1.24 |
| 10 | 40 | 2275 | 7191 | 89.57 | 36.08 | 1.45 |
| 10 | 45 | 2560 | 7361 | 99.27 | 40.08 | 1.57 |

