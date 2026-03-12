--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-12 10:38:31.195146507 UTC |
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
| 1| 5837 | 10.38 | 3.29 | 0.51 |
| 2| 6035 | 12.44 | 3.94 | 0.54 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.91 | 5.98 | 0.64 |
| 10| 7646 | 28.92 | 9.11 | 0.79 |
| 43| 14281 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10070 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 18.71 | 5.86 | 0.37 |
| 2 | 114 | 636 | 25.18 | 7.82 | 0.44 |
| 3 | 170 | 747 | 32.10 | 9.93 | 0.51 |
| 4 | 226 | 858 | 37.79 | 11.71 | 0.58 |
| 5 | 282 | 969 | 42.54 | 13.28 | 0.63 |
| 6 | 338 | 1081 | 52.07 | 16.07 | 0.73 |
| 7 | 394 | 1196 | 59.40 | 18.22 | 0.81 |
| 8 | 450 | 1303 | 63.05 | 19.58 | 0.86 |
| 9 | 504 | 1414 | 75.33 | 23.06 | 0.99 |
| 10 | 560 | 1525 | 92.36 | 27.68 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 18.10 | 6.30 | 0.42 |
| 2| 1950 | 19.19 | 7.29 | 0.44 |
| 3| 2071 | 20.05 | 8.23 | 0.46 |
| 5| 2374 | 23.04 | 10.46 | 0.52 |
| 10| 3198 | 30.84 | 16.13 | 0.66 |
| 50| 9250 | 88.75 | 59.96 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 608 | 16.63 | 5.99 | 0.35 |
| 2| 830 | 18.75 | 7.27 | 0.39 |
| 3| 948 | 19.79 | 8.27 | 0.41 |
| 5| 1316 | 22.81 | 10.49 | 0.47 |
| 10| 1929 | 27.79 | 15.28 | 0.57 |
| 50| 8174 | 88.24 | 59.92 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 20.72 | 6.94 | 0.40 |
| 2| 771 | 21.43 | 7.81 | 0.41 |
| 3| 987 | 23.88 | 9.23 | 0.45 |
| 5| 1252 | 27.94 | 11.74 | 0.52 |
| 10| 2142 | 35.10 | 17.28 | 0.65 |
| 49| 8008 | 95.09 | 61.14 | 1.71 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 702 | 25.34 | 8.25 | 0.45 |
| 2| 765 | 26.36 | 9.20 | 0.46 |
| 3| 896 | 27.90 | 10.32 | 0.49 |
| 5| 1389 | 33.10 | 13.25 | 0.57 |
| 10| 2265 | 43.16 | 19.60 | 0.74 |
| 41| 6834 | 97.48 | 56.20 | 1.64 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5697 | 20.22 | 6.96 | 0.61 |
| 2| 5937 | 31.26 | 11.01 | 0.74 |
| 3| 6063 | 38.83 | 13.73 | 0.83 |
| 4| 6294 | 47.58 | 16.85 | 0.93 |
| 5| 6347 | 54.58 | 19.32 | 1.01 |
| 6| 6599 | 62.71 | 22.24 | 1.11 |
| 7| 6804 | 74.08 | 26.32 | 1.24 |
| 8| 6895 | 77.84 | 27.66 | 1.28 |
| 9| 6905 | 83.93 | 29.50 | 1.34 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.83 | 5.83 | 0.58 |
| 10 | 20 | 1139 | 6514 | 53.51 | 21.08 | 1.02 |
| 10 | 45 | 2559 | 7361 | 99.52 | 40.16 | 1.57 |

