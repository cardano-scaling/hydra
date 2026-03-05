--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-05 13:10:46.600137356 UTC |
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
| 2| 6038 | 12.42 | 3.93 | 0.54 |
| 3| 6238 | 15.07 | 4.78 | 0.58 |
| 5| 6640 | 18.83 | 5.95 | 0.64 |
| 10| 7646 | 29.40 | 9.28 | 0.79 |
| 43| 14282 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2162 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 114 | 636 | 24.19 | 7.58 | 0.43 |
| 3 | 169 | 747 | 29.98 | 9.38 | 0.49 |
| 4 | 227 | 862 | 37.15 | 11.54 | 0.57 |
| 5 | 283 | 974 | 42.67 | 13.31 | 0.63 |
| 6 | 338 | 1081 | 52.91 | 16.22 | 0.74 |
| 7 | 394 | 1192 | 60.74 | 18.58 | 0.83 |
| 8 | 449 | 1303 | 66.00 | 20.27 | 0.89 |
| 9 | 504 | 1414 | 78.26 | 23.63 | 1.02 |
| 10 | 561 | 1525 | 85.34 | 26.00 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 17.84 | 6.24 | 0.42 |
| 2| 1978 | 19.88 | 7.50 | 0.45 |
| 3| 2114 | 21.07 | 8.52 | 0.47 |
| 5| 2412 | 23.06 | 10.46 | 0.52 |
| 10| 3139 | 29.87 | 15.84 | 0.64 |
| 50| 9113 | 87.67 | 59.66 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 16.61 | 5.98 | 0.36 |
| 2| 729 | 17.77 | 6.98 | 0.38 |
| 3| 917 | 19.31 | 8.08 | 0.40 |
| 5| 1157 | 20.76 | 9.84 | 0.44 |
| 10| 2102 | 30.50 | 16.13 | 0.61 |
| 50| 8285 | 91.80 | 60.99 | 1.70 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 21.88 | 7.27 | 0.41 |
| 2| 800 | 23.28 | 8.35 | 0.43 |
| 3| 1058 | 24.43 | 9.41 | 0.46 |
| 5| 1209 | 25.82 | 11.12 | 0.49 |
| 10| 2093 | 37.01 | 17.82 | 0.67 |
| 49| 8005 | 93.91 | 60.76 | 1.70 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 25.34 | 8.25 | 0.45 |
| 2| 765 | 26.33 | 9.19 | 0.46 |
| 3| 1008 | 29.01 | 10.68 | 0.51 |
| 5| 1221 | 31.53 | 12.73 | 0.55 |
| 10| 1957 | 40.20 | 18.65 | 0.70 |
| 42| 6731 | 96.96 | 56.66 | 1.64 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5813 | 23.75 | 8.33 | 0.66 |
| 2| 5966 | 31.32 | 11.06 | 0.75 |
| 3| 6112 | 39.84 | 14.11 | 0.84 |
| 4| 6199 | 43.65 | 15.37 | 0.89 |
| 5| 6483 | 56.84 | 20.14 | 1.04 |
| 6| 6534 | 64.91 | 22.98 | 1.13 |
| 7| 6617 | 70.65 | 24.97 | 1.19 |
| 8| 6677 | 73.26 | 25.82 | 1.22 |
| 9| 6856 | 83.75 | 29.33 | 1.34 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6004 | 25.85 | 9.60 | 0.69 |
| 10 | 10 | 569 | 6173 | 35.29 | 13.51 | 0.80 |
| 10 | 20 | 1138 | 6512 | 51.85 | 20.50 | 1.00 |
| 10 | 30 | 1709 | 6856 | 71.50 | 28.57 | 1.24 |
| 10 | 40 | 2277 | 7193 | 89.98 | 36.23 | 1.45 |
| 10 | 45 | 2561 | 7362 | 99.10 | 40.02 | 1.56 |

