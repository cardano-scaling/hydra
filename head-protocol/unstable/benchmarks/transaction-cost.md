--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-04 15:54:18.777299667 UTC |
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
| 1| 5836 | 10.48 | 3.33 | 0.52 |
| 2| 6037 | 12.99 | 4.13 | 0.55 |
| 3| 6236 | 14.47 | 4.57 | 0.57 |
| 5| 6641 | 19.02 | 6.02 | 0.64 |
| 10| 7650 | 29.18 | 9.20 | 0.79 |
| 43| 14279 | 98.78 | 30.87 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10043 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.31 | 9.40 | 0.51 |
| 3 | 169 | 747 | 43.76 | 12.53 | 0.63 |
| 4 | 226 | 858 | 49.21 | 14.23 | 0.69 |
| 5 | 283 | 969 | 64.37 | 18.28 | 0.84 |
| 6 | 338 | 1081 | 64.74 | 18.78 | 0.86 |
| 7 | 394 | 1192 | 78.97 | 22.59 | 1.00 |
| 8 | 449 | 1303 | 80.64 | 23.33 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 23.92 | 7.60 | 0.48 |
| 2| 1981 | 27.00 | 9.10 | 0.52 |
| 3| 2190 | 28.89 | 10.32 | 0.55 |
| 5| 2480 | 33.48 | 12.92 | 0.62 |
| 10| 3245 | 43.35 | 19.00 | 0.78 |
| 41| 7664 | 97.58 | 54.74 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 615 | 22.57 | 7.31 | 0.41 |
| 2| 813 | 25.14 | 8.70 | 0.45 |
| 3| 937 | 26.14 | 9.61 | 0.47 |
| 5| 1192 | 29.90 | 12.01 | 0.53 |
| 10| 2292 | 44.98 | 19.57 | 0.76 |
| 43| 6877 | 99.64 | 56.80 | 1.67 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 673 | 27.54 | 8.47 | 0.46 |
| 2| 778 | 30.87 | 10.05 | 0.51 |
| 3| 952 | 33.47 | 11.45 | 0.55 |
| 5| 1126 | 35.52 | 13.32 | 0.58 |
| 10| 2034 | 48.53 | 20.37 | 0.78 |
| 38| 6000 | 99.00 | 53.15 | 1.60 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 674 | 33.83 | 10.15 | 0.53 |
| 2| 764 | 35.17 | 11.17 | 0.55 |
| 3| 998 | 38.63 | 12.83 | 0.60 |
| 5| 1295 | 43.36 | 15.49 | 0.67 |
| 10| 2038 | 54.32 | 21.89 | 0.84 |
| 29| 4828 | 97.95 | 46.66 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 27.09 | 9.10 | 0.69 |
| 2| 5889 | 32.57 | 10.88 | 0.75 |
| 3| 6080 | 44.96 | 15.11 | 0.89 |
| 4| 6328 | 56.11 | 18.92 | 1.02 |
| 5| 6382 | 64.21 | 21.63 | 1.11 |
| 6| 6573 | 70.68 | 23.81 | 1.18 |
| 7| 6548 | 73.90 | 24.77 | 1.21 |
| 8| 6862 | 91.66 | 30.79 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 20.07 | 6.71 | 0.62 |
| 10 | 20 | 1137 | 6512 | 59.10 | 22.22 | 1.07 |
| 10 | 39 | 2221 | 7160 | 97.16 | 37.28 | 1.52 |

