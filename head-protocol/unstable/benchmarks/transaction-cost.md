--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-01-30 12:06:36.110587383 UTC |
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
| 1| 5837 | 10.59 | 3.36 | 0.52 |
| 2| 6037 | 12.63 | 4.00 | 0.55 |
| 3| 6238 | 14.52 | 4.59 | 0.58 |
| 5| 6640 | 18.43 | 5.81 | 0.63 |
| 10| 7647 | 29.12 | 9.18 | 0.79 |
| 43| 14279 | 99.16 | 31.00 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 34.19 | 9.84 | 0.53 |
| 3 | 169 | 747 | 43.79 | 12.52 | 0.63 |
| 4 | 226 | 858 | 53.57 | 15.30 | 0.73 |
| 5 | 282 | 969 | 62.05 | 17.66 | 0.82 |
| 6 | 338 | 1081 | 71.82 | 20.54 | 0.93 |
| 7 | 395 | 1192 | 79.67 | 22.65 | 1.01 |
| 8 | 449 | 1303 | 86.98 | 24.89 | 1.09 |
| 9 | 505 | 1414 | 88.42 | 25.69 | 1.11 |
| 10 | 561 | 1525 | 95.72 | 27.66 | 1.19 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1810 | 23.92 | 7.60 | 0.48 |
| 2| 1944 | 25.43 | 8.68 | 0.50 |
| 3| 2205 | 28.89 | 10.32 | 0.56 |
| 5| 2486 | 32.95 | 12.79 | 0.62 |
| 10| 3110 | 40.65 | 18.27 | 0.75 |
| 40| 7770 | 98.91 | 54.49 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 22.81 | 7.37 | 0.42 |
| 2| 699 | 22.55 | 7.95 | 0.42 |
| 3| 976 | 26.12 | 9.62 | 0.47 |
| 5| 1307 | 32.13 | 12.64 | 0.56 |
| 10| 1983 | 39.14 | 17.93 | 0.68 |
| 42| 6590 | 99.47 | 55.95 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 666 | 29.13 | 8.90 | 0.48 |
| 2| 779 | 30.90 | 10.06 | 0.51 |
| 3| 944 | 30.98 | 10.76 | 0.52 |
| 5| 1242 | 36.91 | 13.75 | 0.60 |
| 10| 2155 | 46.25 | 19.78 | 0.76 |
| 37| 6158 | 99.69 | 52.78 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 938 | 37.95 | 12.63 | 0.59 |
| 5| 1338 | 43.35 | 15.49 | 0.67 |
| 10| 2042 | 53.94 | 21.78 | 0.83 |
| 29| 4957 | 98.54 | 46.86 | 1.50 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5809 | 27.08 | 9.08 | 0.69 |
| 2| 5942 | 35.88 | 12.06 | 0.79 |
| 3| 6040 | 43.83 | 14.66 | 0.88 |
| 4| 6233 | 53.90 | 18.13 | 0.99 |
| 5| 6253 | 55.74 | 18.61 | 1.01 |
| 6| 6475 | 70.12 | 23.62 | 1.17 |
| 7| 6776 | 81.73 | 27.51 | 1.31 |
| 8| 6906 | 93.10 | 31.35 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.38 | 6.48 | 0.61 |
| 10 | 10 | 567 | 6171 | 38.62 | 14.15 | 0.84 |
| 10 | 20 | 1137 | 6512 | 59.54 | 22.38 | 1.08 |
| 10 | 39 | 2221 | 7161 | 98.93 | 37.88 | 1.54 |

