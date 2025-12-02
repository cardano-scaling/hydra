--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-02 11:53:20.367785198 UTC |
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
| 1| 5836 | 11.02 | 3.52 | 0.52 |
| 2| 6038 | 13.08 | 4.16 | 0.55 |
| 3| 6236 | 14.52 | 4.59 | 0.58 |
| 5| 6646 | 18.58 | 5.86 | 0.64 |
| 10| 7646 | 28.94 | 9.11 | 0.79 |
| 43| 14283 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 922 | 4.36 | 2.33 | 0.24 |
| 5| 1284 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10072 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 640 | 34.34 | 9.90 | 0.53 |
| 3 | 169 | 747 | 42.62 | 12.26 | 0.62 |
| 4 | 228 | 858 | 52.33 | 14.95 | 0.72 |
| 5 | 282 | 969 | 59.82 | 17.17 | 0.80 |
| 6 | 338 | 1081 | 68.37 | 19.64 | 0.89 |
| 7 | 394 | 1192 | 78.53 | 22.43 | 1.00 |
| 8 | 452 | 1303 | 90.02 | 25.68 | 1.12 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1748 | 22.92 | 7.32 | 0.47 |
| 2| 1944 | 25.84 | 8.78 | 0.51 |
| 3| 2011 | 26.24 | 9.56 | 0.52 |
| 5| 2425 | 32.37 | 12.61 | 0.61 |
| 10| 3194 | 41.69 | 18.56 | 0.76 |
| 41| 7686 | 99.78 | 55.34 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 22.81 | 7.37 | 0.42 |
| 2| 745 | 23.61 | 8.24 | 0.43 |
| 3| 830 | 24.02 | 9.02 | 0.45 |
| 5| 1218 | 30.02 | 12.04 | 0.53 |
| 10| 2054 | 40.67 | 18.36 | 0.70 |
| 44| 6986 | 99.41 | 57.36 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.54 | 8.47 | 0.46 |
| 2| 766 | 30.94 | 10.07 | 0.51 |
| 3| 941 | 32.75 | 11.24 | 0.54 |
| 5| 1176 | 36.24 | 13.54 | 0.59 |
| 10| 2073 | 45.54 | 19.56 | 0.75 |
| 34| 5481 | 95.73 | 49.53 | 1.52 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 679 | 33.83 | 10.15 | 0.53 |
| 2| 814 | 35.85 | 11.38 | 0.56 |
| 3| 960 | 37.91 | 12.62 | 0.59 |
| 5| 1258 | 42.65 | 15.28 | 0.66 |
| 10| 1925 | 52.60 | 21.37 | 0.81 |
| 30| 4988 | 99.22 | 47.71 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5790 | 27.04 | 9.08 | 0.69 |
| 2| 5933 | 36.08 | 12.13 | 0.79 |
| 3| 5992 | 41.52 | 13.89 | 0.85 |
| 4| 6200 | 53.82 | 18.07 | 0.99 |
| 5| 6241 | 58.45 | 19.53 | 1.04 |
| 6| 6680 | 75.24 | 25.41 | 1.24 |
| 7| 6823 | 84.20 | 28.39 | 1.34 |
| 8| 6923 | 95.20 | 32.18 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 56 | 5868 | 20.78 | 7.06 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1139 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 39 | 2221 | 7161 | 98.24 | 37.65 | 1.53 |

