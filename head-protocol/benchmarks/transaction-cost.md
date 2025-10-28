--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 19:17:35.576075835 UTC |
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
| 1| 5834 | 10.48 | 3.33 | 0.52 |
| 2| 6037 | 12.65 | 4.01 | 0.55 |
| 3| 6238 | 14.97 | 4.75 | 0.58 |
| 5| 6641 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 28.81 | 9.07 | 0.78 |
| 43| 14279 | 98.76 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10051 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 171 | 747 | 41.24 | 11.91 | 0.60 |
| 4 | 227 | 858 | 53.71 | 15.28 | 0.73 |
| 5 | 281 | 969 | 56.48 | 16.40 | 0.77 |
| 6 | 337 | 1085 | 72.98 | 20.67 | 0.94 |
| 7 | 393 | 1196 | 73.25 | 21.30 | 0.95 |
| 8 | 449 | 1303 | 94.05 | 26.64 | 1.16 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1803 | 24.37 | 7.71 | 0.48 |
| 2| 1879 | 24.43 | 8.40 | 0.49 |
| 3| 2077 | 27.06 | 9.80 | 0.53 |
| 5| 2397 | 31.03 | 12.25 | 0.59 |
| 10| 3185 | 41.64 | 18.55 | 0.76 |
| 43| 7813 | 98.36 | 56.28 | 1.69 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 22.77 | 7.36 | 0.42 |
| 2| 700 | 22.55 | 7.95 | 0.42 |
| 3| 904 | 25.83 | 9.54 | 0.47 |
| 5| 1090 | 27.15 | 11.22 | 0.50 |
| 10| 1857 | 36.59 | 17.20 | 0.65 |
| 41| 6693 | 97.80 | 54.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 27.51 | 8.47 | 0.46 |
| 2| 733 | 30.27 | 9.86 | 0.50 |
| 3| 944 | 30.94 | 10.75 | 0.52 |
| 5| 1297 | 34.97 | 13.23 | 0.59 |
| 10| 1890 | 46.02 | 19.61 | 0.75 |
| 36| 6000 | 98.23 | 51.68 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 693 | 33.83 | 10.16 | 0.53 |
| 2| 874 | 36.52 | 11.59 | 0.57 |
| 3| 896 | 37.13 | 12.38 | 0.58 |
| 5| 1244 | 42.65 | 15.28 | 0.66 |
| 10| 2145 | 55.47 | 22.24 | 0.85 |
| 30| 4954 | 99.07 | 47.65 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5815 | 27.05 | 9.07 | 0.69 |
| 2| 5822 | 31.45 | 10.47 | 0.74 |
| 3| 6041 | 43.56 | 14.60 | 0.87 |
| 4| 6227 | 53.78 | 18.06 | 0.99 |
| 5| 6334 | 60.84 | 20.43 | 1.07 |
| 6| 6595 | 72.32 | 24.36 | 1.20 |
| 7| 6765 | 83.70 | 28.29 | 1.33 |
| 8| 6754 | 85.86 | 28.84 | 1.35 |
| 9| 7005 | 95.87 | 32.22 | 1.47 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.63 | 6.56 | 0.61 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 20 | 1137 | 6511 | 60.42 | 22.68 | 1.09 |
| 10 | 30 | 1706 | 6853 | 79.60 | 30.31 | 1.31 |
| 10 | 38 | 2162 | 7125 | 97.95 | 37.44 | 1.53 |

