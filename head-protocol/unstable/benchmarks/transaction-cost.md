--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-01 13:09:59.589844601 UTC |
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
| 1| 5837 | 10.35 | 3.28 | 0.51 |
| 2| 6037 | 12.23 | 3.86 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6641 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 28.90 | 9.10 | 0.79 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1282 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10048 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.29 | 5.75 | 0.36 |
| 2 | 114 | 636 | 24.19 | 7.58 | 0.43 |
| 3 | 169 | 747 | 30.89 | 9.64 | 0.50 |
| 4 | 227 | 858 | 37.08 | 11.55 | 0.57 |
| 5 | 283 | 969 | 42.24 | 13.18 | 0.63 |
| 6 | 337 | 1081 | 51.37 | 15.87 | 0.73 |
| 7 | 395 | 1196 | 56.43 | 17.50 | 0.78 |
| 8 | 451 | 1303 | 63.65 | 19.74 | 0.86 |
| 9 | 505 | 1414 | 75.51 | 23.15 | 0.99 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1797 | 17.84 | 6.24 | 0.42 |
| 2| 1922 | 19.19 | 7.29 | 0.44 |
| 3| 2064 | 20.33 | 8.30 | 0.47 |
| 5| 2519 | 24.66 | 10.94 | 0.54 |
| 10| 3124 | 29.69 | 15.76 | 0.64 |
| 50| 9254 | 87.74 | 59.71 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 16.64 | 5.98 | 0.36 |
| 2| 845 | 18.75 | 7.28 | 0.39 |
| 3| 958 | 20.67 | 8.53 | 0.42 |
| 5| 1183 | 20.81 | 9.86 | 0.44 |
| 10| 2102 | 30.69 | 16.19 | 0.61 |
| 50| 8040 | 87.54 | 59.70 | 1.64 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 651 | 21.91 | 7.28 | 0.41 |
| 2| 781 | 23.26 | 8.34 | 0.43 |
| 3| 1004 | 23.86 | 9.22 | 0.45 |
| 5| 1217 | 26.11 | 11.22 | 0.50 |
| 10| 2100 | 35.00 | 17.26 | 0.65 |
| 50| 8287 | 98.02 | 62.68 | 1.76 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 692 | 25.34 | 8.25 | 0.44 |
| 2| 889 | 27.46 | 9.56 | 0.48 |
| 3| 1001 | 28.98 | 10.67 | 0.51 |
| 5| 1205 | 31.40 | 12.70 | 0.55 |
| 10| 2103 | 41.77 | 19.14 | 0.72 |
| 42| 6869 | 98.50 | 57.15 | 1.66 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5789 | 23.82 | 8.36 | 0.66 |
| 2| 5983 | 32.30 | 11.42 | 0.76 |
| 3| 6167 | 40.00 | 14.17 | 0.85 |
| 4| 6344 | 48.57 | 17.27 | 0.95 |
| 5| 6470 | 56.18 | 19.96 | 1.03 |
| 6| 6649 | 62.42 | 22.15 | 1.11 |
| 7| 6732 | 73.50 | 25.96 | 1.23 |
| 8| 6737 | 74.79 | 26.41 | 1.24 |
| 9| 7028 | 91.70 | 32.25 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.83 | 5.83 | 0.58 |
| 10 | 1 | 56 | 5868 | 19.05 | 6.73 | 0.61 |
| 10 | 5 | 285 | 6005 | 25.03 | 9.31 | 0.68 |
| 10 | 10 | 569 | 6173 | 35.29 | 13.51 | 0.80 |
| 10 | 20 | 1137 | 6512 | 52.68 | 20.79 | 1.01 |
| 10 | 40 | 2277 | 7194 | 88.74 | 35.79 | 1.44 |
| 10 | 45 | 2564 | 7365 | 98.86 | 39.93 | 1.56 |

