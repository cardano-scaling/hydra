--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-27 12:20:55.553367272 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6035 | 12.65 | 4.01 | 0.55 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10065 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.25 | 5.74 | 0.36 |
| 2 | 114 | 636 | 24.08 | 7.54 | 0.43 |
| 3 | 171 | 747 | 30.59 | 9.54 | 0.50 |
| 4 | 227 | 858 | 36.30 | 11.37 | 0.56 |
| 5 | 283 | 969 | 43.55 | 13.53 | 0.64 |
| 6 | 338 | 1081 | 51.44 | 15.95 | 0.73 |
| 7 | 393 | 1192 | 56.71 | 17.57 | 0.79 |
| 8 | 449 | 1303 | 62.89 | 19.43 | 0.86 |
| 9 | 505 | 1414 | 80.22 | 24.22 | 1.04 |
| 10 | 560 | 1525 | 81.55 | 24.98 | 1.06 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 17.89 | 6.25 | 0.42 |
| 2| 1983 | 19.65 | 7.45 | 0.45 |
| 3| 2126 | 21.07 | 8.52 | 0.48 |
| 5| 2456 | 23.94 | 10.72 | 0.53 |
| 10| 3235 | 31.77 | 16.40 | 0.67 |
| 50| 9354 | 89.69 | 60.25 | 1.72 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 16.77 | 6.02 | 0.36 |
| 2| 755 | 17.73 | 6.97 | 0.38 |
| 3| 882 | 18.83 | 7.97 | 0.40 |
| 5| 1300 | 23.69 | 10.76 | 0.48 |
| 10| 1953 | 28.84 | 15.63 | 0.58 |
| 50| 8202 | 90.00 | 60.46 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 21.91 | 7.28 | 0.41 |
| 2| 800 | 23.28 | 8.35 | 0.43 |
| 3| 940 | 24.64 | 9.42 | 0.46 |
| 5| 1269 | 26.41 | 11.31 | 0.50 |
| 10| 2069 | 34.61 | 17.12 | 0.64 |
| 49| 7801 | 92.80 | 60.40 | 1.68 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 25.32 | 8.25 | 0.44 |
| 2| 806 | 26.92 | 9.38 | 0.47 |
| 3| 953 | 28.44 | 10.50 | 0.50 |
| 5| 1208 | 31.43 | 12.71 | 0.55 |
| 10| 1966 | 40.38 | 18.69 | 0.70 |
| 41| 6713 | 96.29 | 55.86 | 1.63 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 23.81 | 8.36 | 0.66 |
| 2| 5997 | 32.10 | 11.35 | 0.76 |
| 3| 6085 | 37.02 | 13.08 | 0.81 |
| 4| 6207 | 46.91 | 16.60 | 0.92 |
| 5| 6528 | 57.69 | 20.51 | 1.05 |
| 6| 6528 | 64.22 | 22.77 | 1.12 |
| 7| 6663 | 68.96 | 24.45 | 1.18 |
| 8| 6838 | 84.38 | 29.49 | 1.35 |
| 9| 7066 | 85.83 | 30.44 | 1.37 |
| 10| 7360 | 97.39 | 34.61 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.41 | 5.68 | 0.58 |
| 10 | 10 | 570 | 6175 | 35.29 | 13.51 | 0.80 |
| 10 | 20 | 1139 | 6513 | 52.85 | 20.85 | 1.01 |
| 10 | 30 | 1708 | 6854 | 70.50 | 28.22 | 1.22 |
| 10 | 40 | 2277 | 7194 | 89.57 | 36.08 | 1.45 |
| 10 | 44 | 2504 | 7329 | 97.69 | 39.40 | 1.55 |

