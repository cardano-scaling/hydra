--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-24 09:33:09.545683161 UTC |
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
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6243 | 14.50 | 4.58 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 28.73 | 9.04 | 0.78 |
| 43| 14285 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10087 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 113 | 636 | 24.19 | 7.58 | 0.43 |
| 3 | 171 | 747 | 30.28 | 9.49 | 0.50 |
| 4 | 226 | 862 | 36.91 | 11.48 | 0.57 |
| 5 | 282 | 969 | 46.76 | 14.31 | 0.67 |
| 6 | 340 | 1081 | 48.17 | 15.02 | 0.70 |
| 7 | 393 | 1196 | 61.07 | 18.71 | 0.83 |
| 8 | 449 | 1303 | 71.67 | 21.72 | 0.94 |
| 9 | 504 | 1414 | 74.02 | 22.70 | 0.97 |
| 10 | 560 | 1525 | 95.45 | 28.44 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1818 | 18.05 | 6.29 | 0.42 |
| 2| 1926 | 18.98 | 7.24 | 0.44 |
| 3| 2101 | 20.81 | 8.46 | 0.47 |
| 5| 2384 | 22.94 | 10.43 | 0.52 |
| 10| 2985 | 27.98 | 15.26 | 0.62 |
| 50| 9140 | 87.30 | 59.52 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 16.79 | 6.02 | 0.36 |
| 2| 798 | 17.91 | 7.03 | 0.38 |
| 3| 903 | 18.58 | 7.86 | 0.40 |
| 5| 1236 | 22.12 | 10.28 | 0.46 |
| 10| 2111 | 30.17 | 16.03 | 0.60 |
| 50| 7835 | 83.34 | 58.43 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 676 | 21.91 | 7.28 | 0.41 |
| 2| 873 | 23.80 | 8.52 | 0.44 |
| 3| 948 | 23.29 | 9.04 | 0.45 |
| 5| 1130 | 26.85 | 11.39 | 0.50 |
| 10| 1992 | 35.98 | 17.48 | 0.65 |
| 50| 7861 | 99.20 | 62.85 | 1.75 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 25.37 | 8.26 | 0.45 |
| 2| 880 | 27.44 | 9.55 | 0.48 |
| 3| 1008 | 29.01 | 10.68 | 0.51 |
| 5| 1226 | 31.53 | 12.73 | 0.55 |
| 10| 2054 | 40.85 | 18.85 | 0.71 |
| 41| 6679 | 96.94 | 56.06 | 1.63 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5828 | 23.67 | 8.32 | 0.66 |
| 2| 5774 | 24.92 | 8.57 | 0.67 |
| 3| 6171 | 40.06 | 14.21 | 0.85 |
| 4| 6254 | 48.59 | 17.26 | 0.94 |
| 5| 6381 | 52.11 | 18.45 | 0.99 |
| 6| 6483 | 60.12 | 21.25 | 1.07 |
| 7| 6708 | 70.33 | 24.87 | 1.19 |
| 8| 6968 | 83.60 | 29.58 | 1.35 |
| 9| 6960 | 86.78 | 30.78 | 1.38 |
| 10| 7076 | 94.07 | 33.15 | 1.46 |
| 11| 6972 | 93.59 | 32.73 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 17.00 | 5.89 | 0.59 |
| 10 | 1 | 57 | 5868 | 19.46 | 6.87 | 0.62 |
| 10 | 5 | 284 | 6004 | 25.03 | 9.31 | 0.68 |
| 10 | 20 | 1136 | 6510 | 53.51 | 21.08 | 1.02 |
| 10 | 40 | 2276 | 7193 | 89.57 | 36.08 | 1.45 |
| 10 | 45 | 2563 | 7364 | 98.27 | 39.73 | 1.55 |

