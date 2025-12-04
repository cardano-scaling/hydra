--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-04 11:35:47.710332747 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | a1442faf26d4ec409e2f62a685c1d4893f8d6bcbaf7bcb59d6fa1340 | 14599 | 
| μHead | fd173b993e12103cd734ca6710d364e17120a5eb37a224c64ab2b188* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5841 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.80 | 4.07 | 0.55 |
| 3| 6240 | 14.50 | 4.58 | 0.58 |
| 5| 6638 | 18.84 | 5.95 | 0.64 |
| 10| 7650 | 29.26 | 9.23 | 0.79 |
| 43| 14282 | 98.75 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2176 | 12.13 | 7.25 | 0.40 |
| 54| 10073 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 640 | 32.19 | 9.36 | 0.51 |
| 3 | 170 | 747 | 43.84 | 12.56 | 0.63 |
| 4 | 226 | 858 | 48.16 | 13.95 | 0.68 |
| 5 | 283 | 969 | 61.02 | 17.42 | 0.81 |
| 6 | 337 | 1085 | 69.86 | 19.93 | 0.91 |
| 7 | 395 | 1196 | 81.20 | 23.20 | 1.03 |
| 8 | 449 | 1303 | 88.47 | 25.20 | 1.10 |
| 9 | 506 | 1414 | 89.10 | 25.80 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 23.92 | 7.60 | 0.48 |
| 2| 1881 | 24.85 | 8.50 | 0.50 |
| 3| 2013 | 26.36 | 9.59 | 0.52 |
| 5| 2318 | 30.00 | 11.96 | 0.58 |
| 10| 3161 | 40.76 | 18.29 | 0.75 |
| 41| 7735 | 99.48 | 55.28 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.81 | 7.37 | 0.42 |
| 2| 830 | 25.13 | 8.70 | 0.45 |
| 3| 876 | 25.12 | 9.32 | 0.46 |
| 5| 1190 | 29.18 | 11.81 | 0.52 |
| 10| 1865 | 37.24 | 17.41 | 0.66 |
| 41| 6590 | 97.07 | 54.72 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 644 | 29.17 | 8.91 | 0.48 |
| 2| 843 | 29.86 | 9.81 | 0.50 |
| 3| 959 | 30.94 | 10.75 | 0.52 |
| 5| 1259 | 35.34 | 13.34 | 0.59 |
| 10| 1975 | 44.49 | 19.24 | 0.74 |
| 35| 5689 | 93.44 | 49.64 | 1.51 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 33.87 | 10.16 | 0.53 |
| 2| 881 | 36.64 | 11.62 | 0.57 |
| 3| 984 | 38.63 | 12.83 | 0.60 |
| 5| 1328 | 43.24 | 15.47 | 0.67 |
| 10| 1973 | 53.57 | 21.65 | 0.83 |
| 30| 4936 | 99.78 | 47.83 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5819 | 27.12 | 9.10 | 0.69 |
| 2| 5917 | 34.75 | 11.63 | 0.78 |
| 3| 5989 | 41.44 | 13.86 | 0.85 |
| 4| 6180 | 50.25 | 16.84 | 0.95 |
| 5| 6504 | 65.89 | 22.24 | 1.13 |
| 6| 6406 | 68.33 | 22.89 | 1.15 |
| 7| 6700 | 82.98 | 27.96 | 1.32 |
| 8| 6727 | 91.09 | 30.66 | 1.40 |
| 9| 7020 | 99.84 | 33.73 | 1.51 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 29.72 | 10.56 | 0.73 |
| 10 | 10 | 570 | 6174 | 38.18 | 14.00 | 0.83 |
| 10 | 20 | 1135 | 6509 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 79.34 | 30.22 | 1.31 |
| 10 | 39 | 2223 | 7163 | 98.49 | 37.73 | 1.53 |

