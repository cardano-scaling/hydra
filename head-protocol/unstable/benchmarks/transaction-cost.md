--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-16 16:55:37.910575364 UTC |
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
| 1| 5834 | 10.61 | 3.37 | 0.52 |
| 2| 6037 | 12.42 | 3.93 | 0.54 |
| 3| 6239 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.71 | 5.91 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10054 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 113 | 636 | 24.61 | 7.68 | 0.43 |
| 3 | 169 | 747 | 31.15 | 9.66 | 0.50 |
| 4 | 227 | 858 | 38.29 | 11.81 | 0.58 |
| 5 | 281 | 969 | 43.93 | 13.57 | 0.65 |
| 6 | 339 | 1081 | 54.90 | 16.79 | 0.76 |
| 7 | 393 | 1192 | 58.39 | 18.01 | 0.80 |
| 8 | 449 | 1303 | 69.62 | 21.24 | 0.92 |
| 9 | 507 | 1414 | 77.81 | 23.67 | 1.01 |
| 10 | 560 | 1525 | 84.96 | 25.86 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1749 | 17.13 | 6.02 | 0.41 |
| 2| 1882 | 18.43 | 7.06 | 0.43 |
| 3| 2017 | 19.33 | 8.01 | 0.45 |
| 5| 2393 | 23.02 | 10.45 | 0.52 |
| 10| 3272 | 31.45 | 16.32 | 0.67 |
| 50| 9128 | 86.15 | 59.22 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 642 | 16.64 | 5.98 | 0.36 |
| 2| 809 | 18.71 | 7.26 | 0.39 |
| 3| 853 | 17.81 | 7.63 | 0.39 |
| 5| 1235 | 21.49 | 10.08 | 0.45 |
| 10| 2045 | 29.28 | 15.74 | 0.59 |
| 50| 8083 | 86.32 | 59.34 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 21.93 | 7.28 | 0.41 |
| 2| 823 | 23.85 | 8.53 | 0.44 |
| 3| 945 | 23.32 | 9.05 | 0.45 |
| 5| 1413 | 27.62 | 11.70 | 0.52 |
| 10| 2194 | 37.43 | 17.96 | 0.68 |
| 49| 8207 | 98.50 | 62.21 | 1.76 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 24.80 | 8.08 | 0.44 |
| 2| 811 | 26.92 | 9.38 | 0.47 |
| 3| 939 | 28.37 | 10.47 | 0.50 |
| 5| 1260 | 32.02 | 12.90 | 0.56 |
| 10| 2062 | 41.46 | 19.05 | 0.71 |
| 42| 6945 | 99.44 | 57.48 | 1.67 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 23.81 | 8.35 | 0.66 |
| 2| 5994 | 32.26 | 11.40 | 0.76 |
| 3| 6065 | 36.77 | 12.97 | 0.81 |
| 4| 6406 | 48.95 | 17.45 | 0.95 |
| 5| 6245 | 48.12 | 16.90 | 0.94 |
| 6| 6475 | 60.70 | 21.47 | 1.08 |
| 7| 6803 | 73.74 | 26.17 | 1.23 |
| 8| 6880 | 85.37 | 29.95 | 1.36 |
| 9| 7152 | 95.55 | 33.45 | 1.48 |
| 10| 7080 | 91.28 | 32.17 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6005 | 25.44 | 9.45 | 0.69 |
| 10 | 10 | 569 | 6174 | 35.71 | 13.65 | 0.81 |
| 10 | 20 | 1138 | 6512 | 54.33 | 21.37 | 1.03 |
| 10 | 40 | 2275 | 7192 | 89.15 | 35.94 | 1.45 |
| 10 | 44 | 2505 | 7329 | 97.28 | 39.26 | 1.54 |

