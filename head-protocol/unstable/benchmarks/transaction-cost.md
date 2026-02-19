--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-19 14:13:59.227347384 UTC |
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
| 1| 5837 | 10.48 | 3.33 | 0.52 |
| 2| 6037 | 12.54 | 3.97 | 0.55 |
| 3| 6238 | 14.67 | 4.64 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.99 | 30.94 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 741 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.46 | 7.13 | 0.42 |
| 2 | 113 | 636 | 32.39 | 9.43 | 0.51 |
| 3 | 171 | 747 | 41.46 | 11.98 | 0.60 |
| 4 | 226 | 858 | 51.17 | 14.72 | 0.71 |
| 5 | 282 | 969 | 64.61 | 18.31 | 0.85 |
| 6 | 338 | 1081 | 63.79 | 18.50 | 0.85 |
| 7 | 395 | 1192 | 74.88 | 21.60 | 0.96 |
| 8 | 449 | 1303 | 85.03 | 24.43 | 1.07 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 23.92 | 7.60 | 0.48 |
| 2| 1966 | 26.55 | 9.00 | 0.52 |
| 3| 2122 | 28.09 | 10.09 | 0.54 |
| 5| 2397 | 31.29 | 12.31 | 0.60 |
| 10| 3256 | 42.82 | 18.87 | 0.78 |
| 39| 7647 | 99.75 | 54.01 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.80 | 7.37 | 0.41 |
| 2| 762 | 24.05 | 8.39 | 0.44 |
| 3| 915 | 25.76 | 9.53 | 0.47 |
| 5| 1325 | 31.12 | 12.35 | 0.55 |
| 10| 1939 | 37.96 | 17.62 | 0.67 |
| 40| 6509 | 96.30 | 53.86 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 27.47 | 8.46 | 0.46 |
| 2| 879 | 29.97 | 9.84 | 0.50 |
| 3| 903 | 30.23 | 10.54 | 0.51 |
| 5| 1260 | 37.77 | 14.00 | 0.61 |
| 10| 2005 | 44.97 | 19.39 | 0.74 |
| 35| 5993 | 97.74 | 50.92 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 701 | 33.79 | 10.15 | 0.53 |
| 2| 764 | 35.21 | 11.18 | 0.55 |
| 3| 962 | 37.91 | 12.62 | 0.59 |
| 5| 1376 | 43.95 | 15.68 | 0.68 |
| 10| 2081 | 54.85 | 22.04 | 0.84 |
| 28| 4846 | 96.42 | 45.63 | 1.47 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5796 | 27.04 | 9.08 | 0.69 |
| 2| 5931 | 35.95 | 12.09 | 0.79 |
| 3| 6097 | 44.69 | 15.03 | 0.89 |
| 4| 6235 | 52.56 | 17.72 | 0.98 |
| 5| 6428 | 61.17 | 20.65 | 1.08 |
| 6| 6525 | 73.12 | 24.59 | 1.21 |
| 7| 6857 | 85.94 | 29.02 | 1.36 |
| 8| 6893 | 90.11 | 30.35 | 1.40 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 19.89 | 6.76 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.46 | 10.13 | 0.72 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1709 | 6855 | 80.67 | 30.67 | 1.32 |
| 10 | 38 | 2162 | 7124 | 96.44 | 36.92 | 1.51 |

