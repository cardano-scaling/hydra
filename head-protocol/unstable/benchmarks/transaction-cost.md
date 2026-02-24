--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-24 14:41:10.63072633 UTC |
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
| 1| 5837 | 10.66 | 3.39 | 0.52 |
| 2| 6039 | 12.44 | 3.94 | 0.54 |
| 3| 6239 | 14.98 | 4.75 | 0.58 |
| 5| 6645 | 18.62 | 5.87 | 0.64 |
| 10| 7647 | 29.09 | 9.17 | 0.79 |
| 43| 14282 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10062 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 18.71 | 5.86 | 0.37 |
| 2 | 114 | 636 | 24.08 | 7.54 | 0.43 |
| 3 | 170 | 747 | 30.12 | 9.43 | 0.49 |
| 4 | 224 | 858 | 36.18 | 11.33 | 0.56 |
| 5 | 283 | 969 | 42.74 | 13.28 | 0.63 |
| 6 | 339 | 1081 | 54.75 | 16.75 | 0.76 |
| 7 | 395 | 1192 | 61.46 | 18.73 | 0.83 |
| 8 | 449 | 1303 | 69.37 | 21.20 | 0.92 |
| 9 | 505 | 1414 | 79.09 | 23.94 | 1.02 |
| 10 | 560 | 1525 | 91.28 | 27.51 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 18.10 | 6.30 | 0.42 |
| 2| 1946 | 18.98 | 7.24 | 0.44 |
| 3| 2014 | 19.57 | 8.07 | 0.46 |
| 5| 2441 | 23.98 | 10.73 | 0.53 |
| 10| 3147 | 30.14 | 15.91 | 0.65 |
| 50| 9010 | 87.20 | 59.48 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 620 | 16.81 | 6.04 | 0.36 |
| 2| 745 | 17.90 | 7.02 | 0.38 |
| 3| 913 | 18.63 | 7.87 | 0.40 |
| 5| 1190 | 20.84 | 9.87 | 0.44 |
| 10| 1790 | 26.23 | 14.81 | 0.55 |
| 50| 8291 | 89.24 | 60.21 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 20.74 | 6.95 | 0.40 |
| 2| 861 | 22.58 | 8.18 | 0.43 |
| 3| 997 | 23.84 | 9.22 | 0.45 |
| 5| 1168 | 25.28 | 10.95 | 0.48 |
| 10| 2030 | 33.49 | 16.77 | 0.63 |
| 50| 7923 | 94.43 | 61.50 | 1.71 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 25.37 | 8.26 | 0.44 |
| 2| 822 | 26.87 | 9.37 | 0.47 |
| 3| 955 | 28.44 | 10.50 | 0.50 |
| 5| 1246 | 32.10 | 12.92 | 0.56 |
| 10| 2000 | 40.95 | 18.88 | 0.71 |
| 43| 7004 | 99.59 | 58.16 | 1.68 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5817 | 23.81 | 8.36 | 0.66 |
| 2| 5968 | 32.28 | 11.42 | 0.76 |
| 3| 6188 | 39.72 | 14.08 | 0.85 |
| 4| 6279 | 47.60 | 16.91 | 0.93 |
| 5| 6422 | 55.13 | 19.53 | 1.02 |
| 6| 6741 | 66.63 | 23.73 | 1.16 |
| 7| 6743 | 74.97 | 26.56 | 1.24 |
| 8| 6744 | 75.75 | 26.78 | 1.25 |
| 9| 7106 | 88.09 | 31.17 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.00 | 5.89 | 0.59 |
| 10 | 1 | 57 | 5868 | 19.05 | 6.73 | 0.61 |
| 10 | 5 | 284 | 6004 | 25.03 | 9.31 | 0.68 |
| 10 | 10 | 569 | 6173 | 35.47 | 13.57 | 0.81 |
| 10 | 20 | 1140 | 6514 | 53.51 | 21.08 | 1.02 |
| 10 | 40 | 2278 | 7194 | 89.15 | 35.94 | 1.45 |
| 10 | 45 | 2563 | 7364 | 98.69 | 39.87 | 1.56 |

