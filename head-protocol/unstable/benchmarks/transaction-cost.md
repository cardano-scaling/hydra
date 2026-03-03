--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-03 17:07:54.112941687 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6035 | 12.46 | 3.94 | 0.54 |
| 3| 6238 | 14.40 | 4.55 | 0.57 |
| 5| 6638 | 18.41 | 5.80 | 0.63 |
| 10| 7647 | 28.90 | 9.10 | 0.79 |
| 43| 14281 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2172 | 12.13 | 7.25 | 0.40 |
| 54| 10057 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 114 | 636 | 24.10 | 7.55 | 0.43 |
| 3 | 168 | 746 | 31.49 | 9.77 | 0.51 |
| 4 | 228 | 858 | 39.24 | 12.07 | 0.59 |
| 5 | 282 | 969 | 45.60 | 14.09 | 0.66 |
| 6 | 338 | 1085 | 48.52 | 15.14 | 0.70 |
| 7 | 397 | 1192 | 55.98 | 17.40 | 0.78 |
| 8 | 450 | 1303 | 68.70 | 20.97 | 0.91 |
| 9 | 506 | 1414 | 79.85 | 24.14 | 1.03 |
| 10 | 560 | 1525 | 78.88 | 24.25 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1807 | 18.10 | 6.30 | 0.42 |
| 2| 1950 | 19.01 | 7.25 | 0.44 |
| 3| 2053 | 20.12 | 8.25 | 0.46 |
| 5| 2447 | 23.80 | 10.69 | 0.53 |
| 10| 3065 | 29.62 | 15.74 | 0.64 |
| 50| 9111 | 88.41 | 59.85 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 16.64 | 5.98 | 0.36 |
| 2| 741 | 17.53 | 6.88 | 0.37 |
| 3| 828 | 17.91 | 7.65 | 0.39 |
| 5| 1179 | 21.78 | 10.19 | 0.45 |
| 10| 2146 | 31.71 | 16.47 | 0.62 |
| 50| 8306 | 91.24 | 60.81 | 1.69 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 21.91 | 7.28 | 0.41 |
| 2| 736 | 22.74 | 8.17 | 0.43 |
| 3| 912 | 24.66 | 9.42 | 0.46 |
| 5| 1130 | 26.85 | 11.39 | 0.50 |
| 10| 2018 | 33.90 | 16.90 | 0.63 |
| 50| 8309 | 98.48 | 62.89 | 1.76 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 690 | 25.37 | 8.26 | 0.45 |
| 2| 810 | 26.90 | 9.38 | 0.47 |
| 3| 979 | 28.91 | 10.65 | 0.50 |
| 5| 1230 | 31.43 | 12.71 | 0.55 |
| 10| 2032 | 40.79 | 18.83 | 0.71 |
| 41| 6571 | 95.07 | 55.45 | 1.61 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 20.18 | 6.94 | 0.61 |
| 2| 5937 | 31.24 | 11.02 | 0.74 |
| 3| 6093 | 38.90 | 13.74 | 0.83 |
| 4| 6301 | 47.53 | 16.84 | 0.93 |
| 5| 6593 | 57.54 | 20.51 | 1.05 |
| 6| 6507 | 63.61 | 22.56 | 1.11 |
| 7| 6776 | 74.94 | 26.61 | 1.25 |
| 8| 6870 | 78.39 | 27.82 | 1.29 |
| 9| 6912 | 83.05 | 29.45 | 1.34 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.48 | 6.41 | 0.60 |
| 10 | 5 | 284 | 6003 | 25.20 | 9.37 | 0.69 |
| 10 | 20 | 1140 | 6514 | 52.85 | 20.85 | 1.01 |
| 10 | 30 | 1706 | 6852 | 71.74 | 28.65 | 1.24 |
| 10 | 40 | 2276 | 7192 | 88.91 | 35.85 | 1.44 |
| 10 | 45 | 2563 | 7365 | 98.86 | 39.93 | 1.56 |

