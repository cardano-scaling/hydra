--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-05 14:53:32.483848525 UTC |
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
| 1| 5837 | 10.17 | 3.22 | 0.51 |
| 2| 6039 | 12.67 | 4.01 | 0.55 |
| 3| 6236 | 14.69 | 4.65 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14285 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2181 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 114 | 636 | 24.74 | 7.72 | 0.43 |
| 3 | 171 | 747 | 31.30 | 9.71 | 0.51 |
| 4 | 226 | 858 | 39.15 | 12.05 | 0.59 |
| 5 | 284 | 969 | 44.52 | 13.72 | 0.65 |
| 6 | 337 | 1085 | 52.21 | 16.10 | 0.74 |
| 7 | 394 | 1192 | 61.66 | 18.80 | 0.84 |
| 8 | 450 | 1303 | 71.83 | 21.74 | 0.94 |
| 9 | 505 | 1414 | 77.28 | 23.64 | 1.01 |
| 10 | 560 | 1525 | 83.84 | 25.60 | 1.08 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 17.84 | 6.24 | 0.42 |
| 2| 1882 | 18.25 | 7.02 | 0.43 |
| 3| 2115 | 21.02 | 8.51 | 0.47 |
| 5| 2318 | 22.59 | 10.31 | 0.51 |
| 10| 3255 | 31.75 | 16.40 | 0.67 |
| 50| 9387 | 89.98 | 60.36 | 1.73 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 16.77 | 6.01 | 0.36 |
| 2| 722 | 16.75 | 6.64 | 0.37 |
| 3| 995 | 20.53 | 8.47 | 0.42 |
| 5| 1207 | 21.49 | 10.06 | 0.45 |
| 10| 1950 | 27.81 | 15.29 | 0.57 |
| 50| 8003 | 86.30 | 59.32 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 21.88 | 7.27 | 0.41 |
| 2| 820 | 23.85 | 8.53 | 0.44 |
| 3| 963 | 23.24 | 9.03 | 0.45 |
| 5| 1215 | 27.91 | 11.73 | 0.51 |
| 10| 2100 | 36.35 | 17.61 | 0.66 |
| 48| 7450 | 96.16 | 60.57 | 1.69 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 25.34 | 8.25 | 0.44 |
| 2| 815 | 26.90 | 9.38 | 0.47 |
| 3| 1049 | 29.50 | 10.84 | 0.51 |
| 5| 1383 | 33.20 | 13.28 | 0.58 |
| 10| 2030 | 41.16 | 18.95 | 0.71 |
| 43| 7013 | 99.83 | 58.22 | 1.69 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 23.75 | 8.33 | 0.66 |
| 2| 5964 | 32.25 | 11.41 | 0.76 |
| 3| 6060 | 38.81 | 13.73 | 0.83 |
| 4| 6335 | 48.29 | 17.15 | 0.94 |
| 5| 6458 | 55.56 | 19.78 | 1.03 |
| 6| 6567 | 58.57 | 20.70 | 1.06 |
| 7| 6778 | 74.35 | 26.28 | 1.24 |
| 8| 6793 | 73.80 | 26.05 | 1.23 |
| 9| 7109 | 87.21 | 31.03 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6005 | 25.44 | 9.45 | 0.69 |
| 10 | 20 | 1139 | 6513 | 53.51 | 21.08 | 1.02 |
| 10 | 40 | 2279 | 7195 | 89.57 | 36.08 | 1.45 |
| 10 | 45 | 2563 | 7365 | 97.45 | 39.44 | 1.55 |

