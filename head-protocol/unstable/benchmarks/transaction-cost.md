--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-23 16:52:10.605470025 UTC |
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
| 1| 5838 | 10.74 | 3.42 | 0.52 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6239 | 14.38 | 4.54 | 0.57 |
| 5| 6640 | 18.84 | 5.95 | 0.64 |
| 10| 7644 | 29.40 | 9.28 | 0.79 |
| 43| 14283 | 98.97 | 30.93 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2178 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.29 | 5.75 | 0.36 |
| 2 | 114 | 636 | 24.15 | 7.57 | 0.43 |
| 3 | 170 | 747 | 30.59 | 9.54 | 0.50 |
| 4 | 226 | 858 | 37.78 | 11.73 | 0.58 |
| 5 | 282 | 969 | 46.84 | 14.34 | 0.67 |
| 6 | 341 | 1081 | 49.15 | 15.31 | 0.71 |
| 7 | 396 | 1192 | 63.34 | 19.29 | 0.85 |
| 8 | 449 | 1303 | 63.46 | 19.65 | 0.86 |
| 9 | 504 | 1418 | 77.30 | 23.58 | 1.01 |
| 10 | 560 | 1525 | 89.52 | 27.17 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 17.34 | 6.07 | 0.41 |
| 2| 1948 | 18.98 | 7.24 | 0.44 |
| 3| 2057 | 20.12 | 8.25 | 0.46 |
| 5| 2417 | 23.95 | 10.73 | 0.53 |
| 10| 3203 | 30.37 | 15.99 | 0.65 |
| 50| 9336 | 91.26 | 60.72 | 1.74 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 16.64 | 5.98 | 0.36 |
| 2| 772 | 17.49 | 6.87 | 0.38 |
| 3| 985 | 20.72 | 8.52 | 0.42 |
| 5| 1161 | 20.81 | 9.86 | 0.44 |
| 10| 2055 | 30.23 | 16.05 | 0.60 |
| 50| 7918 | 83.77 | 58.61 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 21.91 | 7.28 | 0.41 |
| 2| 770 | 21.48 | 7.82 | 0.41 |
| 3| 939 | 24.92 | 9.51 | 0.46 |
| 5| 1202 | 25.92 | 11.15 | 0.49 |
| 10| 1896 | 34.60 | 17.05 | 0.64 |
| 50| 8192 | 97.71 | 62.60 | 1.75 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 25.34 | 8.25 | 0.44 |
| 2| 850 | 27.46 | 9.56 | 0.48 |
| 3| 1016 | 29.01 | 10.68 | 0.51 |
| 5| 1250 | 32.02 | 12.90 | 0.56 |
| 10| 2064 | 41.64 | 19.11 | 0.72 |
| 42| 6936 | 98.86 | 57.29 | 1.67 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5820 | 23.82 | 8.36 | 0.66 |
| 2| 5823 | 27.59 | 9.62 | 0.70 |
| 3| 6065 | 38.96 | 13.77 | 0.83 |
| 4| 6162 | 43.86 | 15.46 | 0.89 |
| 5| 6238 | 48.10 | 16.91 | 0.94 |
| 6| 6459 | 63.60 | 22.54 | 1.11 |
| 7| 6801 | 74.26 | 26.46 | 1.24 |
| 8| 6770 | 78.49 | 27.56 | 1.28 |
| 9| 7072 | 86.85 | 30.87 | 1.39 |
| 10| 7108 | 96.17 | 33.79 | 1.48 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5868 | 20.70 | 7.31 | 0.63 |
| 10 | 5 | 285 | 6005 | 26.03 | 9.66 | 0.69 |
| 10 | 10 | 569 | 6174 | 35.29 | 13.51 | 0.80 |
| 10 | 20 | 1139 | 6514 | 53.51 | 21.08 | 1.02 |
| 10 | 30 | 1707 | 6853 | 71.74 | 28.65 | 1.24 |
| 10 | 40 | 2279 | 7196 | 89.98 | 36.23 | 1.46 |
| 10 | 44 | 2504 | 7328 | 97.45 | 39.32 | 1.54 |

