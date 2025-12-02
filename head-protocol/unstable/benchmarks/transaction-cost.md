--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 11:35:31.775702358 UTC |
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
| 1| 5838 | 10.85 | 3.45 | 0.52 |
| 2| 6039 | 12.23 | 3.86 | 0.54 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6638 | 18.60 | 5.87 | 0.63 |
| 10| 7646 | 29.00 | 9.14 | 0.79 |
| 43| 14279 | 98.64 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2163 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 170 | 747 | 43.96 | 12.60 | 0.63 |
| 4 | 226 | 858 | 53.86 | 15.34 | 0.73 |
| 5 | 283 | 969 | 61.06 | 17.46 | 0.81 |
| 6 | 340 | 1085 | 68.13 | 19.62 | 0.89 |
| 7 | 394 | 1196 | 78.50 | 22.47 | 1.00 |
| 8 | 449 | 1307 | 94.37 | 26.77 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1746 | 22.92 | 7.32 | 0.47 |
| 2| 1930 | 25.80 | 8.77 | 0.51 |
| 3| 2055 | 27.02 | 9.79 | 0.53 |
| 5| 2440 | 32.41 | 12.62 | 0.61 |
| 10| 3100 | 40.78 | 18.30 | 0.75 |
| 42| 7721 | 98.66 | 55.71 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.57 | 7.32 | 0.41 |
| 2| 768 | 23.59 | 8.23 | 0.43 |
| 3| 940 | 26.95 | 9.86 | 0.48 |
| 5| 1271 | 30.17 | 12.07 | 0.54 |
| 10| 1895 | 37.60 | 17.51 | 0.66 |
| 40| 6644 | 97.78 | 54.27 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 689 | 27.54 | 8.47 | 0.46 |
| 2| 871 | 29.89 | 9.82 | 0.50 |
| 3| 865 | 32.09 | 11.03 | 0.53 |
| 5| 1310 | 35.76 | 13.47 | 0.59 |
| 10| 2080 | 48.98 | 20.50 | 0.79 |
| 35| 5906 | 96.03 | 50.42 | 1.55 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 678 | 33.83 | 10.15 | 0.53 |
| 2| 810 | 35.92 | 11.40 | 0.56 |
| 3| 1021 | 38.51 | 12.80 | 0.60 |
| 5| 1375 | 44.07 | 15.71 | 0.68 |
| 10| 2102 | 55.20 | 22.17 | 0.85 |
| 30| 4867 | 98.80 | 47.53 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5807 | 27.00 | 9.07 | 0.69 |
| 2| 5820 | 31.60 | 10.50 | 0.74 |
| 3| 6129 | 46.19 | 15.56 | 0.91 |
| 4| 6306 | 54.98 | 18.50 | 1.01 |
| 5| 6532 | 65.94 | 22.29 | 1.13 |
| 6| 6431 | 70.16 | 23.60 | 1.17 |
| 7| 6665 | 82.71 | 27.84 | 1.31 |
| 8| 6753 | 89.85 | 30.27 | 1.39 |
| 9| 6995 | 99.65 | 33.48 | 1.50 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 10 | 570 | 6175 | 39.06 | 14.30 | 0.84 |
| 10 | 20 | 1139 | 6513 | 60.35 | 22.66 | 1.09 |
| 10 | 30 | 1709 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 38 | 2164 | 7126 | 96.88 | 37.08 | 1.51 |

