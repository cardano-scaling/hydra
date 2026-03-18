--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-18 14:00:23.68275979 UTC |
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
| 2| 6037 | 12.61 | 4.00 | 0.55 |
| 3| 6242 | 14.47 | 4.57 | 0.57 |
| 5| 6641 | 18.50 | 5.83 | 0.63 |
| 10| 7644 | 28.73 | 9.04 | 0.78 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10069 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.67 | 5.84 | 0.37 |
| 2 | 113 | 636 | 25.20 | 7.82 | 0.44 |
| 3 | 169 | 747 | 32.35 | 10.02 | 0.52 |
| 4 | 226 | 858 | 38.38 | 11.83 | 0.58 |
| 5 | 283 | 969 | 42.31 | 13.20 | 0.63 |
| 6 | 336 | 1081 | 52.08 | 16.00 | 0.73 |
| 7 | 392 | 1196 | 59.62 | 18.36 | 0.82 |
| 8 | 448 | 1307 | 69.54 | 21.18 | 0.92 |
| 9 | 506 | 1414 | 75.24 | 23.03 | 0.99 |
| 10 | 560 | 1525 | 88.44 | 26.68 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1809 | 18.10 | 6.30 | 0.42 |
| 2| 1992 | 19.77 | 7.48 | 0.45 |
| 3| 2055 | 20.28 | 8.29 | 0.46 |
| 5| 2346 | 22.54 | 10.29 | 0.51 |
| 10| 3317 | 32.67 | 16.68 | 0.68 |
| 50| 9091 | 88.66 | 59.90 | 1.70 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 16.79 | 6.02 | 0.36 |
| 2| 744 | 17.92 | 7.04 | 0.38 |
| 3| 990 | 19.79 | 8.26 | 0.41 |
| 5| 1345 | 23.72 | 10.76 | 0.48 |
| 10| 1937 | 28.33 | 15.47 | 0.58 |
| 50| 8196 | 88.95 | 60.16 | 1.66 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 20.72 | 6.94 | 0.40 |
| 2| 770 | 21.43 | 7.81 | 0.41 |
| 3| 906 | 22.70 | 8.85 | 0.44 |
| 5| 1214 | 27.89 | 11.73 | 0.51 |
| 10| 1862 | 32.40 | 16.40 | 0.61 |
| 49| 7954 | 95.40 | 61.24 | 1.71 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 25.34 | 8.25 | 0.44 |
| 2| 807 | 26.87 | 9.37 | 0.47 |
| 3| 996 | 28.95 | 10.66 | 0.50 |
| 5| 1401 | 33.17 | 13.27 | 0.58 |
| 10| 2124 | 41.34 | 19.01 | 0.71 |
| 42| 6938 | 99.40 | 57.45 | 1.67 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5787 | 23.78 | 8.34 | 0.66 |
| 2| 5868 | 30.34 | 10.65 | 0.73 |
| 3| 6149 | 40.04 | 14.21 | 0.85 |
| 4| 6237 | 44.44 | 15.68 | 0.90 |
| 5| 6477 | 56.14 | 19.98 | 1.03 |
| 6| 6534 | 63.92 | 22.61 | 1.12 |
| 7| 6742 | 70.96 | 25.12 | 1.20 |
| 8| 6749 | 77.84 | 27.34 | 1.27 |
| 9| 6718 | 81.58 | 28.51 | 1.31 |
| 10| 7165 | 98.36 | 34.60 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.41 | 5.68 | 0.58 |
| 10 | 1 | 57 | 5869 | 18.63 | 6.58 | 0.61 |
| 10 | 5 | 283 | 6002 | 26.27 | 9.74 | 0.70 |
| 10 | 10 | 568 | 6172 | 34.23 | 13.13 | 0.79 |
| 10 | 20 | 1136 | 6510 | 52.27 | 20.64 | 1.01 |
| 10 | 40 | 2276 | 7192 | 88.74 | 35.79 | 1.44 |
| 10 | 45 | 2560 | 7362 | 99.27 | 40.08 | 1.57 |

