--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-22 21:03:52.185049823 UTC |
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
| 1| 5836 | 10.67 | 3.39 | 0.52 |
| 2| 6042 | 12.44 | 3.94 | 0.55 |
| 3| 6238 | 14.29 | 4.51 | 0.57 |
| 5| 6640 | 19.02 | 6.02 | 0.64 |
| 10| 7646 | 28.73 | 9.04 | 0.78 |
| 43| 14281 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10063 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.71 | 5.86 | 0.37 |
| 2 | 113 | 636 | 24.74 | 7.72 | 0.43 |
| 3 | 171 | 747 | 30.73 | 9.56 | 0.50 |
| 4 | 225 | 858 | 37.68 | 11.65 | 0.58 |
| 5 | 283 | 969 | 42.57 | 13.26 | 0.63 |
| 6 | 341 | 1081 | 52.25 | 16.10 | 0.74 |
| 7 | 396 | 1192 | 57.90 | 17.87 | 0.80 |
| 8 | 450 | 1303 | 67.80 | 20.80 | 0.91 |
| 9 | 505 | 1414 | 78.52 | 23.75 | 1.02 |
| 10 | 560 | 1525 | 84.97 | 25.86 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 17.89 | 6.25 | 0.42 |
| 2| 1929 | 18.98 | 7.24 | 0.44 |
| 3| 2013 | 19.57 | 8.07 | 0.46 |
| 5| 2422 | 23.94 | 10.72 | 0.53 |
| 10| 3171 | 30.25 | 15.94 | 0.65 |
| 50| 9301 | 88.17 | 59.84 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 16.65 | 5.99 | 0.36 |
| 2| 748 | 17.92 | 7.04 | 0.38 |
| 3| 887 | 18.59 | 7.87 | 0.40 |
| 5| 1266 | 22.28 | 10.31 | 0.46 |
| 10| 1981 | 28.58 | 15.52 | 0.58 |
| 50| 8167 | 90.19 | 60.47 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 694 | 20.74 | 6.95 | 0.40 |
| 2| 785 | 23.31 | 8.36 | 0.43 |
| 3| 955 | 25.21 | 9.60 | 0.47 |
| 5| 1266 | 26.49 | 11.33 | 0.50 |
| 10| 2011 | 36.04 | 17.49 | 0.66 |
| 50| 7816 | 93.50 | 61.24 | 1.69 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 680 | 25.34 | 8.25 | 0.44 |
| 2| 883 | 27.41 | 9.55 | 0.48 |
| 3| 986 | 29.03 | 10.69 | 0.51 |
| 5| 1304 | 32.07 | 12.91 | 0.56 |
| 10| 2187 | 42.29 | 19.31 | 0.73 |
| 41| 6664 | 95.88 | 55.71 | 1.62 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5796 | 23.75 | 8.35 | 0.66 |
| 2| 5986 | 32.22 | 11.41 | 0.76 |
| 3| 6106 | 36.81 | 12.98 | 0.81 |
| 4| 6256 | 47.69 | 16.85 | 0.93 |
| 5| 6474 | 56.42 | 20.01 | 1.04 |
| 6| 6604 | 62.02 | 22.06 | 1.10 |
| 7| 6759 | 71.67 | 25.49 | 1.21 |
| 8| 6743 | 74.58 | 26.35 | 1.24 |
| 9| 6964 | 87.93 | 31.06 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.07 | 6.26 | 0.60 |
| 10 | 1 | 56 | 5867 | 17.81 | 6.29 | 0.60 |
| 10 | 5 | 285 | 6005 | 26.27 | 9.74 | 0.70 |
| 10 | 10 | 570 | 6174 | 36.12 | 13.80 | 0.81 |
| 10 | 20 | 1139 | 6514 | 52.68 | 20.79 | 1.01 |
| 10 | 30 | 1709 | 6855 | 70.50 | 28.22 | 1.22 |
| 10 | 40 | 2275 | 7191 | 89.98 | 36.23 | 1.45 |
| 10 | 44 | 2505 | 7329 | 95.79 | 38.74 | 1.53 |

