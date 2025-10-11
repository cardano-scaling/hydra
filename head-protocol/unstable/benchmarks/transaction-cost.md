--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-11 07:24:43.666503391 UTC |
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
| 1| 5837 | 10.57 | 3.36 | 0.52 |
| 2| 6037 | 12.41 | 3.92 | 0.54 |
| 3| 6238 | 14.67 | 4.64 | 0.58 |
| 5| 6638 | 18.72 | 5.91 | 0.64 |
| 10| 7647 | 29.14 | 9.19 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 559 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1279 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10060 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 170 | 747 | 40.12 | 11.65 | 0.59 |
| 4 | 227 | 862 | 52.58 | 15.04 | 0.72 |
| 5 | 282 | 969 | 62.75 | 17.84 | 0.83 |
| 6 | 340 | 1081 | 73.17 | 20.79 | 0.94 |
| 7 | 392 | 1192 | 77.09 | 22.18 | 0.99 |
| 8 | 450 | 1303 | 90.91 | 25.73 | 1.13 |
| 9 | 504 | 1414 | 91.15 | 26.24 | 1.14 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 24.00 | 7.62 | 0.48 |
| 2| 1971 | 26.92 | 9.08 | 0.52 |
| 3| 2118 | 28.02 | 10.07 | 0.54 |
| 5| 2430 | 32.49 | 12.64 | 0.61 |
| 10| 3271 | 43.22 | 18.97 | 0.78 |
| 41| 7718 | 98.35 | 54.97 | 1.68 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 22.77 | 7.36 | 0.41 |
| 2| 808 | 25.33 | 8.74 | 0.45 |
| 3| 874 | 25.58 | 9.49 | 0.46 |
| 5| 1202 | 29.52 | 11.92 | 0.53 |
| 10| 1925 | 37.47 | 17.46 | 0.66 |
| 39| 6365 | 98.18 | 53.65 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 691 | 27.51 | 8.47 | 0.46 |
| 2| 780 | 30.98 | 10.08 | 0.51 |
| 3| 945 | 30.82 | 10.73 | 0.52 |
| 5| 1219 | 37.02 | 13.78 | 0.60 |
| 10| 2073 | 48.07 | 20.24 | 0.78 |
| 36| 6054 | 97.42 | 51.48 | 1.57 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 704 | 33.83 | 10.16 | 0.53 |
| 2| 765 | 35.21 | 11.18 | 0.55 |
| 3| 948 | 37.91 | 12.62 | 0.59 |
| 5| 1371 | 44.00 | 15.69 | 0.68 |
| 10| 1999 | 53.19 | 21.55 | 0.82 |
| 29| 4829 | 98.32 | 46.79 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5792 | 27.00 | 9.06 | 0.69 |
| 2| 5822 | 31.56 | 10.51 | 0.74 |
| 3| 6083 | 46.02 | 15.50 | 0.90 |
| 4| 6304 | 55.94 | 18.89 | 1.02 |
| 5| 6495 | 66.13 | 22.31 | 1.13 |
| 6| 6499 | 70.74 | 23.81 | 1.18 |
| 7| 6695 | 83.91 | 28.28 | 1.33 |
| 8| 6663 | 86.38 | 28.95 | 1.35 |
| 9| 6743 | 88.70 | 29.65 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 18.30 | 6.11 | 0.60 |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 5 | 285 | 6004 | 28.02 | 9.98 | 0.71 |
| 10 | 10 | 570 | 6174 | 39.51 | 14.45 | 0.85 |
| 10 | 39 | 2216 | 7155 | 98.05 | 37.58 | 1.53 |

