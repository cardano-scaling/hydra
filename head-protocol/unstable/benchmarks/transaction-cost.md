--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-05 19:53:58.52911698 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6037 | 12.44 | 3.94 | 0.54 |
| 3| 6236 | 14.98 | 4.75 | 0.58 |
| 5| 6641 | 19.17 | 6.07 | 0.64 |
| 10| 7646 | 29.31 | 9.25 | 0.79 |
| 43| 14279 | 99.04 | 30.96 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.25 | 5.74 | 0.36 |
| 2 | 113 | 640 | 24.15 | 7.57 | 0.43 |
| 3 | 171 | 747 | 32.07 | 9.93 | 0.51 |
| 4 | 225 | 858 | 39.34 | 12.12 | 0.59 |
| 5 | 282 | 969 | 46.41 | 14.20 | 0.67 |
| 6 | 338 | 1081 | 53.82 | 16.48 | 0.75 |
| 7 | 395 | 1196 | 57.20 | 17.66 | 0.79 |
| 8 | 449 | 1303 | 68.48 | 20.96 | 0.91 |
| 9 | 507 | 1414 | 77.70 | 23.58 | 1.01 |
| 10 | 560 | 1525 | 78.29 | 24.07 | 1.02 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1795 | 18.10 | 6.30 | 0.42 |
| 2| 1994 | 19.72 | 7.47 | 0.45 |
| 3| 2109 | 20.81 | 8.46 | 0.47 |
| 5| 2343 | 22.23 | 10.21 | 0.51 |
| 10| 3207 | 31.12 | 16.20 | 0.66 |
| 50| 9249 | 88.63 | 59.95 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 618 | 16.65 | 6.00 | 0.36 |
| 2| 816 | 18.70 | 7.26 | 0.39 |
| 3| 914 | 19.31 | 8.09 | 0.40 |
| 5| 1303 | 22.77 | 10.48 | 0.47 |
| 10| 1991 | 29.95 | 15.96 | 0.59 |
| 50| 8016 | 85.63 | 59.16 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 20.72 | 6.94 | 0.40 |
| 2| 812 | 21.99 | 7.99 | 0.42 |
| 3| 963 | 25.20 | 9.60 | 0.47 |
| 5| 1130 | 26.75 | 11.36 | 0.50 |
| 10| 1984 | 35.93 | 17.47 | 0.65 |
| 50| 7879 | 93.71 | 61.33 | 1.70 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 684 | 25.37 | 8.26 | 0.44 |
| 2| 806 | 26.92 | 9.38 | 0.47 |
| 3| 895 | 27.82 | 10.30 | 0.49 |
| 5| 1157 | 30.94 | 12.54 | 0.54 |
| 10| 1850 | 39.05 | 18.27 | 0.68 |
| 43| 6982 | 99.77 | 58.19 | 1.68 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 23.81 | 8.35 | 0.66 |
| 2| 5978 | 32.27 | 11.42 | 0.76 |
| 3| 6087 | 37.10 | 13.07 | 0.81 |
| 4| 6314 | 47.55 | 16.87 | 0.93 |
| 5| 6315 | 49.09 | 17.30 | 0.95 |
| 6| 6568 | 62.57 | 22.16 | 1.10 |
| 7| 6679 | 69.37 | 24.51 | 1.18 |
| 8| 6910 | 79.25 | 28.14 | 1.30 |
| 9| 7198 | 92.03 | 32.66 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.41 | 5.68 | 0.58 |
| 10 | 1 | 57 | 5869 | 20.29 | 7.16 | 0.62 |
| 10 | 5 | 285 | 6005 | 25.85 | 9.60 | 0.69 |
| 10 | 10 | 569 | 6173 | 34.88 | 13.36 | 0.80 |
| 10 | 20 | 1139 | 6514 | 53.51 | 21.08 | 1.02 |
| 10 | 40 | 2276 | 7192 | 90.81 | 36.52 | 1.46 |
| 10 | 45 | 2561 | 7362 | 99.10 | 40.02 | 1.56 |

