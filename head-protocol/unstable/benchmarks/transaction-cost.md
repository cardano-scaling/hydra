--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-08 18:40:28.839837373 UTC |
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
| 1| 5836 | 10.86 | 3.46 | 0.52 |
| 2| 6037 | 12.42 | 3.93 | 0.54 |
| 3| 6238 | 14.72 | 4.66 | 0.58 |
| 5| 6641 | 18.64 | 5.88 | 0.64 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14283 | 98.64 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2171 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 114 | 636 | 34.27 | 9.87 | 0.53 |
| 3 | 171 | 747 | 41.48 | 11.99 | 0.60 |
| 4 | 227 | 858 | 53.78 | 15.33 | 0.73 |
| 5 | 283 | 974 | 60.74 | 17.35 | 0.81 |
| 6 | 340 | 1081 | 73.18 | 20.76 | 0.94 |
| 7 | 395 | 1192 | 87.27 | 24.62 | 1.09 |
| 8 | 449 | 1307 | 94.25 | 26.74 | 1.16 |
| 9 | 505 | 1414 | 96.87 | 27.67 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.00 | 7.62 | 0.48 |
| 2| 1966 | 26.23 | 8.90 | 0.51 |
| 3| 2158 | 29.38 | 10.44 | 0.56 |
| 5| 2448 | 32.33 | 12.60 | 0.61 |
| 10| 3290 | 44.42 | 19.30 | 0.79 |
| 42| 7870 | 99.95 | 56.11 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 22.53 | 7.32 | 0.41 |
| 2| 782 | 23.59 | 8.23 | 0.43 |
| 3| 865 | 25.58 | 9.48 | 0.46 |
| 5| 1144 | 28.42 | 11.63 | 0.51 |
| 10| 1997 | 39.60 | 18.06 | 0.69 |
| 42| 6695 | 99.57 | 56.07 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.47 | 8.46 | 0.46 |
| 2| 822 | 31.58 | 10.27 | 0.52 |
| 3| 990 | 33.35 | 11.42 | 0.55 |
| 5| 1325 | 38.29 | 14.17 | 0.62 |
| 10| 1883 | 46.06 | 19.62 | 0.75 |
| 36| 5925 | 96.56 | 51.21 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 33.79 | 10.15 | 0.53 |
| 2| 828 | 35.85 | 11.38 | 0.56 |
| 3| 938 | 37.84 | 12.60 | 0.59 |
| 5| 1200 | 41.90 | 15.05 | 0.65 |
| 10| 1930 | 52.52 | 21.35 | 0.81 |
| 30| 4933 | 99.59 | 47.77 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5805 | 27.00 | 9.07 | 0.69 |
| 2| 5925 | 36.04 | 12.09 | 0.79 |
| 3| 6045 | 45.01 | 15.12 | 0.89 |
| 4| 6319 | 56.04 | 18.90 | 1.02 |
| 5| 6318 | 56.85 | 19.01 | 1.02 |
| 6| 6630 | 74.35 | 25.04 | 1.22 |
| 7| 6808 | 81.39 | 27.57 | 1.31 |
| 8| 6762 | 89.89 | 30.26 | 1.39 |
| 9| 6973 | 95.52 | 32.12 | 1.46 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.82 | 6.63 | 0.61 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 20 | 1137 | 6511 | 59.28 | 22.29 | 1.08 |
| 10 | 30 | 1706 | 6853 | 80.92 | 30.76 | 1.33 |
| 10 | 39 | 2221 | 7160 | 98.49 | 37.73 | 1.53 |

