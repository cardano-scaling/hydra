--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-28 11:34:25.854864547 UTC |
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
| 1| 5836 | 10.17 | 3.22 | 0.51 |
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.50 | 4.58 | 0.57 |
| 5| 6638 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 29.02 | 9.14 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2168 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 171 | 747 | 41.39 | 11.97 | 0.60 |
| 4 | 228 | 862 | 52.73 | 15.12 | 0.72 |
| 5 | 283 | 969 | 60.58 | 17.34 | 0.81 |
| 6 | 338 | 1081 | 75.53 | 21.39 | 0.96 |
| 7 | 394 | 1192 | 72.80 | 21.10 | 0.94 |
| 8 | 453 | 1303 | 89.33 | 25.41 | 1.11 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1815 | 24.37 | 7.71 | 0.48 |
| 2| 1923 | 25.84 | 8.78 | 0.51 |
| 3| 2127 | 27.94 | 10.05 | 0.54 |
| 5| 2328 | 30.41 | 12.06 | 0.59 |
| 10| 3091 | 39.84 | 18.03 | 0.74 |
| 41| 7671 | 98.32 | 54.91 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 606 | 22.57 | 7.30 | 0.41 |
| 2| 762 | 23.62 | 8.25 | 0.43 |
| 3| 854 | 24.11 | 9.04 | 0.45 |
| 5| 1203 | 29.99 | 12.03 | 0.53 |
| 10| 1980 | 39.68 | 18.08 | 0.69 |
| 41| 6651 | 97.01 | 54.69 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 29.17 | 8.91 | 0.48 |
| 2| 766 | 28.55 | 9.40 | 0.48 |
| 3| 1071 | 32.36 | 11.19 | 0.54 |
| 5| 1257 | 35.12 | 13.27 | 0.59 |
| 10| 2132 | 45.98 | 19.69 | 0.76 |
| 35| 5808 | 99.94 | 51.41 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 33.15 | 9.95 | 0.52 |
| 2| 872 | 36.64 | 11.62 | 0.57 |
| 3| 987 | 38.59 | 12.82 | 0.60 |
| 5| 1268 | 42.68 | 15.29 | 0.66 |
| 10| 2105 | 55.48 | 22.24 | 0.85 |
| 29| 4869 | 97.28 | 46.51 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 27.00 | 9.08 | 0.69 |
| 2| 5998 | 37.06 | 12.49 | 0.80 |
| 3| 6040 | 41.44 | 13.87 | 0.85 |
| 4| 6275 | 55.19 | 18.62 | 1.01 |
| 5| 6402 | 61.14 | 20.59 | 1.07 |
| 6| 6481 | 69.51 | 23.42 | 1.17 |
| 7| 6789 | 84.49 | 28.54 | 1.34 |
| 8| 6614 | 82.17 | 27.43 | 1.30 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 283 | 6003 | 30.23 | 10.73 | 0.74 |
| 10 | 10 | 569 | 6173 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6854 | 80.22 | 30.52 | 1.32 |
| 10 | 40 | 2276 | 7193 | 99.66 | 38.24 | 1.55 |
| 10 | 40 | 2279 | 7196 | 99.66 | 38.24 | 1.55 |

