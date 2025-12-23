--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-12-23 14:37:07.446597276 UTC |
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
| 1| 5836 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.25 | 3.87 | 0.54 |
| 3| 6236 | 14.48 | 4.58 | 0.57 |
| 5| 6638 | 18.62 | 5.87 | 0.64 |
| 10| 7644 | 28.71 | 9.03 | 0.78 |
| 43| 14281 | 99.32 | 31.06 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10056 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 56 | 528 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.31 | 9.88 | 0.53 |
| 3 | 170 | 747 | 43.76 | 12.53 | 0.63 |
| 4 | 227 | 858 | 47.88 | 13.89 | 0.67 |
| 5 | 284 | 969 | 60.70 | 17.34 | 0.81 |
| 6 | 338 | 1081 | 63.88 | 18.53 | 0.85 |
| 7 | 396 | 1192 | 84.51 | 23.90 | 1.06 |
| 8 | 449 | 1303 | 93.62 | 26.43 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 24.29 | 7.69 | 0.48 |
| 2| 1924 | 25.55 | 8.71 | 0.50 |
| 3| 2067 | 27.02 | 9.79 | 0.53 |
| 5| 2384 | 31.08 | 12.26 | 0.59 |
| 10| 3094 | 39.55 | 17.96 | 0.74 |
| 39| 7425 | 95.06 | 52.71 | 1.62 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 22.50 | 7.30 | 0.41 |
| 2| 822 | 25.52 | 8.80 | 0.46 |
| 3| 928 | 26.90 | 9.84 | 0.48 |
| 5| 1208 | 30.23 | 12.13 | 0.54 |
| 10| 1905 | 37.58 | 17.47 | 0.66 |
| 39| 6602 | 98.79 | 53.85 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 686 | 27.47 | 8.46 | 0.46 |
| 2| 882 | 29.93 | 9.83 | 0.50 |
| 3| 899 | 30.19 | 10.53 | 0.51 |
| 5| 1248 | 35.04 | 13.25 | 0.58 |
| 10| 1937 | 47.06 | 19.93 | 0.76 |
| 34| 5660 | 92.90 | 48.86 | 1.50 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 701 | 33.83 | 10.16 | 0.53 |
| 2| 874 | 36.60 | 11.61 | 0.57 |
| 3| 961 | 37.91 | 12.62 | 0.59 |
| 5| 1200 | 41.86 | 15.04 | 0.65 |
| 10| 1978 | 53.27 | 21.57 | 0.82 |
| 29| 4773 | 97.34 | 46.49 | 1.48 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5779 | 26.97 | 9.05 | 0.69 |
| 2| 5948 | 35.84 | 12.06 | 0.79 |
| 3| 6140 | 46.02 | 15.49 | 0.90 |
| 4| 6255 | 54.68 | 18.44 | 1.00 |
| 5| 6332 | 62.79 | 21.06 | 1.09 |
| 6| 6620 | 75.00 | 25.35 | 1.23 |
| 7| 6650 | 75.68 | 25.49 | 1.24 |
| 8| 6735 | 89.69 | 30.17 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 18.75 | 6.26 | 0.60 |
| 10 | 5 | 284 | 6003 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 567 | 6171 | 39.51 | 14.45 | 0.85 |
| 10 | 20 | 1139 | 6513 | 59.54 | 22.38 | 1.08 |
| 10 | 38 | 2163 | 7125 | 96.63 | 36.99 | 1.51 |

