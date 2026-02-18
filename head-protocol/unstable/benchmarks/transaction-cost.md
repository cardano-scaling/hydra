--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-18 20:17:20.42746931 UTC |
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
| 1| 5837 | 10.78 | 3.43 | 0.52 |
| 2| 6037 | 12.84 | 4.08 | 0.55 |
| 3| 6239 | 14.38 | 4.54 | 0.57 |
| 5| 6646 | 18.43 | 5.81 | 0.63 |
| 10| 7646 | 29.26 | 9.23 | 0.79 |
| 43| 14282 | 98.85 | 30.89 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 737 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2173 | 12.13 | 7.25 | 0.40 |
| 54| 10045 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.61 | 0.52 |
| 3 | 170 | 747 | 43.84 | 12.55 | 0.63 |
| 4 | 226 | 858 | 53.54 | 15.27 | 0.73 |
| 5 | 282 | 969 | 57.03 | 16.60 | 0.77 |
| 6 | 339 | 1081 | 69.68 | 19.88 | 0.90 |
| 7 | 394 | 1192 | 77.03 | 22.20 | 0.99 |
| 8 | 448 | 1303 | 96.41 | 27.16 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 24.37 | 7.71 | 0.48 |
| 2| 1945 | 25.92 | 8.80 | 0.51 |
| 3| 2056 | 27.31 | 9.86 | 0.53 |
| 5| 2372 | 31.34 | 12.32 | 0.60 |
| 10| 3145 | 42.08 | 18.65 | 0.76 |
| 40| 7537 | 96.54 | 53.83 | 1.65 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 603 | 22.57 | 7.31 | 0.41 |
| 2| 807 | 25.56 | 8.81 | 0.46 |
| 3| 916 | 26.87 | 9.85 | 0.48 |
| 5| 1233 | 29.12 | 11.78 | 0.52 |
| 10| 2040 | 41.57 | 18.59 | 0.71 |
| 41| 6735 | 99.33 | 55.33 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.46 | 8.69 | 0.47 |
| 2| 820 | 31.69 | 10.29 | 0.52 |
| 3| 1104 | 32.25 | 11.16 | 0.54 |
| 5| 1292 | 37.74 | 13.99 | 0.61 |
| 10| 2132 | 49.12 | 20.55 | 0.79 |
| 36| 5989 | 98.13 | 51.65 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.87 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 1025 | 38.63 | 12.83 | 0.60 |
| 5| 1307 | 43.31 | 15.48 | 0.67 |
| 10| 2140 | 55.82 | 22.34 | 0.86 |
| 28| 4797 | 97.21 | 45.89 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5824 | 26.96 | 9.06 | 0.69 |
| 2| 5869 | 34.84 | 11.65 | 0.77 |
| 3| 6018 | 43.64 | 14.65 | 0.87 |
| 4| 6249 | 53.84 | 18.10 | 0.99 |
| 5| 6412 | 64.61 | 21.80 | 1.11 |
| 6| 6714 | 74.86 | 25.27 | 1.23 |
| 7| 6751 | 85.29 | 28.80 | 1.34 |
| 8| 6921 | 91.23 | 30.78 | 1.41 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 5 | 285 | 6005 | 29.35 | 10.43 | 0.73 |
| 10 | 39 | 2222 | 7161 | 98.49 | 37.73 | 1.53 |

