--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-04 10:29:39.333813435 UTC |
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
| 1| 5836 | 10.86 | 3.46 | 0.52 |
| 2| 6042 | 12.61 | 4.00 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6640 | 18.81 | 5.94 | 0.64 |
| 10| 7651 | 29.12 | 9.18 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 736 | 3.38 | 1.73 | 0.22 |
| 3| 921 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10049 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 114 | 636 | 25.25 | 7.85 | 0.44 |
| 3 | 169 | 747 | 30.22 | 9.47 | 0.50 |
| 4 | 227 | 862 | 36.90 | 11.51 | 0.57 |
| 5 | 281 | 974 | 45.68 | 14.04 | 0.66 |
| 6 | 339 | 1081 | 50.48 | 15.60 | 0.72 |
| 7 | 394 | 1192 | 63.92 | 19.44 | 0.86 |
| 8 | 449 | 1307 | 66.01 | 20.25 | 0.89 |
| 9 | 504 | 1414 | 76.36 | 23.24 | 1.00 |
| 10 | 561 | 1525 | 93.17 | 27.93 | 1.17 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 17.89 | 6.25 | 0.42 |
| 2| 1927 | 19.24 | 7.31 | 0.44 |
| 3| 2058 | 20.28 | 8.29 | 0.46 |
| 5| 2381 | 23.04 | 10.46 | 0.52 |
| 10| 3053 | 28.77 | 15.49 | 0.63 |
| 50| 9478 | 92.15 | 61.06 | 1.75 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 16.79 | 6.02 | 0.36 |
| 2| 722 | 16.75 | 6.64 | 0.37 |
| 3| 960 | 19.76 | 8.26 | 0.41 |
| 5| 1186 | 21.26 | 10.03 | 0.45 |
| 10| 1909 | 28.40 | 15.49 | 0.58 |
| 50| 8150 | 88.14 | 59.88 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 21.91 | 7.28 | 0.41 |
| 2| 774 | 21.45 | 7.81 | 0.41 |
| 3| 910 | 24.61 | 9.41 | 0.46 |
| 5| 1206 | 25.87 | 11.14 | 0.49 |
| 10| 2045 | 36.22 | 17.58 | 0.66 |
| 50| 7841 | 93.47 | 61.25 | 1.69 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 25.34 | 8.25 | 0.44 |
| 2| 861 | 27.49 | 9.57 | 0.48 |
| 3| 1022 | 28.95 | 10.66 | 0.51 |
| 5| 1268 | 32.09 | 12.92 | 0.56 |
| 10| 2000 | 40.64 | 18.80 | 0.70 |
| 42| 6907 | 99.42 | 57.49 | 1.67 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5804 | 23.75 | 8.33 | 0.66 |
| 2| 5960 | 31.22 | 11.01 | 0.74 |
| 3| 6081 | 39.91 | 14.14 | 0.84 |
| 4| 6308 | 47.56 | 16.87 | 0.93 |
| 5| 6461 | 56.55 | 20.07 | 1.04 |
| 6| 6454 | 63.12 | 22.22 | 1.10 |
| 7| 6928 | 75.90 | 27.04 | 1.26 |
| 8| 6913 | 78.72 | 27.91 | 1.29 |
| 9| 7126 | 97.58 | 33.92 | 1.50 |
| 10| 7121 | 92.06 | 32.62 | 1.44 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 18.63 | 6.58 | 0.61 |
| 10 | 5 | 284 | 6004 | 26.85 | 9.95 | 0.70 |
| 10 | 10 | 569 | 6173 | 34.88 | 13.36 | 0.80 |
| 10 | 20 | 1138 | 6512 | 52.27 | 20.64 | 1.01 |
| 10 | 30 | 1707 | 6853 | 72.15 | 28.80 | 1.24 |
| 10 | 46 | 2620 | 7398 | 98.86 | 40.05 | 1.56 |

