--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-04 09:13:37.650997792 UTC |
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
| 1| 5837 | 10.19 | 3.22 | 0.51 |
| 2| 6038 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.71 | 4.65 | 0.58 |
| 5| 6645 | 18.79 | 5.94 | 0.64 |
| 10| 7648 | 29.00 | 9.14 | 0.79 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1274 | 6.41 | 3.60 | 0.28 |
| 10| 2167 | 12.13 | 7.25 | 0.40 |
| 54| 10059 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 113 | 636 | 34.23 | 9.85 | 0.53 |
| 3 | 170 | 747 | 43.73 | 12.51 | 0.63 |
| 4 | 227 | 858 | 52.15 | 14.94 | 0.72 |
| 5 | 283 | 969 | 58.25 | 16.79 | 0.78 |
| 6 | 339 | 1081 | 74.98 | 21.15 | 0.96 |
| 7 | 394 | 1192 | 83.51 | 23.58 | 1.05 |
| 8 | 450 | 1303 | 83.07 | 23.96 | 1.05 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1794 | 24.37 | 7.71 | 0.48 |
| 2| 1924 | 25.43 | 8.68 | 0.50 |
| 3| 2053 | 26.87 | 9.75 | 0.53 |
| 5| 2419 | 32.40 | 12.62 | 0.61 |
| 10| 3103 | 39.76 | 18.01 | 0.74 |
| 41| 7601 | 97.57 | 54.73 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 22.81 | 7.37 | 0.42 |
| 2| 772 | 24.32 | 8.46 | 0.44 |
| 3| 872 | 25.09 | 9.33 | 0.46 |
| 5| 1254 | 29.94 | 12.03 | 0.53 |
| 10| 1792 | 35.55 | 16.90 | 0.64 |
| 39| 6669 | 99.00 | 53.91 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 28.46 | 8.69 | 0.47 |
| 2| 736 | 30.27 | 9.86 | 0.50 |
| 3| 982 | 33.47 | 11.46 | 0.55 |
| 5| 1213 | 34.22 | 13.01 | 0.57 |
| 10| 1903 | 45.87 | 19.57 | 0.75 |
| 36| 5929 | 96.65 | 51.27 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 33.15 | 9.95 | 0.52 |
| 2| 807 | 35.89 | 11.39 | 0.56 |
| 3| 937 | 37.84 | 12.60 | 0.59 |
| 5| 1249 | 42.57 | 15.26 | 0.66 |
| 10| 1948 | 53.15 | 21.54 | 0.82 |
| 29| 5024 | 98.67 | 46.89 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5820 | 26.96 | 9.06 | 0.69 |
| 2| 5914 | 35.88 | 12.05 | 0.79 |
| 3| 6097 | 44.77 | 15.05 | 0.89 |
| 4| 6214 | 51.34 | 17.26 | 0.96 |
| 5| 6408 | 64.13 | 21.58 | 1.11 |
| 6| 6557 | 73.02 | 24.52 | 1.21 |
| 7| 6763 | 79.19 | 26.62 | 1.28 |
| 8| 6797 | 87.48 | 29.42 | 1.37 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 568 | 6173 | 40.39 | 14.75 | 0.85 |
| 10 | 20 | 1138 | 6512 | 61.31 | 22.98 | 1.10 |
| 10 | 30 | 1709 | 6855 | 80.48 | 30.61 | 1.32 |
| 10 | 40 | 2278 | 7194 | 99.66 | 38.24 | 1.55 |
| 10 | 39 | 2221 | 7161 | 99.38 | 38.04 | 1.54 |

