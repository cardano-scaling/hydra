--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-04 11:24:51.371297253 UTC |
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
| 1| 5834 | 10.36 | 3.28 | 0.51 |
| 2| 6037 | 12.61 | 4.00 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6640 | 19.17 | 6.07 | 0.64 |
| 10| 7646 | 29.09 | 9.17 | 0.79 |
| 43| 14281 | 98.87 | 30.90 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10067 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.18 | 9.60 | 0.52 |
| 3 | 170 | 747 | 42.37 | 12.20 | 0.61 |
| 4 | 225 | 858 | 49.59 | 14.32 | 0.69 |
| 5 | 283 | 969 | 58.71 | 16.89 | 0.79 |
| 6 | 338 | 1081 | 67.92 | 19.50 | 0.89 |
| 7 | 394 | 1192 | 86.95 | 24.54 | 1.08 |
| 8 | 450 | 1303 | 95.82 | 26.96 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1791 | 24.29 | 7.69 | 0.48 |
| 2| 1994 | 26.76 | 9.04 | 0.52 |
| 3| 2053 | 27.28 | 9.85 | 0.53 |
| 5| 2426 | 32.64 | 12.68 | 0.61 |
| 10| 3189 | 41.90 | 18.61 | 0.76 |
| 41| 7685 | 97.83 | 54.86 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 610 | 22.57 | 7.32 | 0.41 |
| 2| 773 | 23.59 | 8.23 | 0.43 |
| 3| 979 | 27.80 | 10.11 | 0.49 |
| 5| 1258 | 29.42 | 11.87 | 0.53 |
| 10| 1835 | 36.39 | 17.16 | 0.65 |
| 40| 6491 | 98.72 | 54.54 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 681 | 27.50 | 8.46 | 0.46 |
| 2| 817 | 29.18 | 9.60 | 0.49 |
| 3| 983 | 33.31 | 11.41 | 0.54 |
| 5| 1225 | 37.03 | 13.78 | 0.60 |
| 10| 1781 | 44.74 | 19.21 | 0.73 |
| 34| 5641 | 99.42 | 50.61 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 679 | 33.83 | 10.15 | 0.53 |
| 2| 833 | 35.85 | 11.38 | 0.56 |
| 3| 951 | 37.91 | 12.62 | 0.59 |
| 5| 1244 | 42.57 | 15.26 | 0.66 |
| 10| 2078 | 55.18 | 22.14 | 0.85 |
| 28| 4699 | 95.47 | 45.32 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 27.09 | 9.08 | 0.69 |
| 2| 5846 | 31.40 | 10.44 | 0.74 |
| 3| 6083 | 42.64 | 14.32 | 0.87 |
| 4| 6193 | 53.64 | 18.07 | 0.99 |
| 5| 6430 | 64.87 | 21.90 | 1.12 |
| 6| 6715 | 76.37 | 25.81 | 1.25 |
| 7| 6712 | 78.81 | 26.52 | 1.27 |
| 8| 6918 | 92.27 | 31.05 | 1.42 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5868 | 20.34 | 6.91 | 0.62 |
| 10 | 10 | 569 | 6173 | 38.62 | 14.15 | 0.84 |
| 10 | 39 | 2218 | 7158 | 98.49 | 37.73 | 1.53 |

