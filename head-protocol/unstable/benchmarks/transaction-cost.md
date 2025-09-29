--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-09-29 04:41:59.066358977 UTC |
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
| 1| 5837 | 10.40 | 3.30 | 0.51 |
| 2| 6038 | 12.72 | 4.03 | 0.55 |
| 3| 6238 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.41 | 5.80 | 0.63 |
| 10| 7644 | 28.94 | 9.11 | 0.79 |
| 43| 14282 | 98.94 | 30.92 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 917 | 4.36 | 2.33 | 0.24 |
| 5| 1283 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10051 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 113 | 636 | 32.24 | 9.37 | 0.51 |
| 3 | 170 | 747 | 41.40 | 11.97 | 0.60 |
| 4 | 226 | 858 | 48.11 | 13.97 | 0.68 |
| 5 | 282 | 969 | 60.84 | 17.41 | 0.81 |
| 6 | 337 | 1081 | 71.50 | 20.32 | 0.92 |
| 7 | 393 | 1192 | 76.68 | 22.03 | 0.98 |
| 8 | 449 | 1303 | 84.85 | 24.39 | 1.07 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1804 | 24.29 | 7.69 | 0.48 |
| 2| 2000 | 26.55 | 9.00 | 0.52 |
| 3| 2055 | 27.32 | 9.86 | 0.53 |
| 5| 2389 | 30.92 | 12.22 | 0.59 |
| 10| 3090 | 39.40 | 17.92 | 0.73 |
| 43| 7934 | 99.56 | 56.64 | 1.71 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 22.84 | 7.40 | 0.42 |
| 2| 795 | 23.55 | 8.22 | 0.43 |
| 3| 947 | 26.67 | 9.79 | 0.48 |
| 5| 1237 | 31.25 | 12.38 | 0.55 |
| 10| 1927 | 37.50 | 17.45 | 0.66 |
| 41| 6470 | 94.47 | 53.98 | 1.59 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 28.46 | 8.69 | 0.47 |
| 2| 737 | 30.27 | 9.86 | 0.50 |
| 3| 1016 | 31.61 | 10.96 | 0.53 |
| 5| 1407 | 38.49 | 14.22 | 0.63 |
| 10| 2089 | 48.36 | 20.33 | 0.78 |
| 38| 6162 | 99.74 | 53.41 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 682 | 33.87 | 10.16 | 0.53 |
| 2| 831 | 35.92 | 11.40 | 0.56 |
| 3| 978 | 38.66 | 12.84 | 0.60 |
| 5| 1276 | 42.57 | 15.26 | 0.66 |
| 10| 2090 | 54.77 | 22.02 | 0.84 |
| 29| 4670 | 95.22 | 45.86 | 1.46 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5797 | 27.00 | 9.06 | 0.69 |
| 2| 5849 | 31.44 | 10.45 | 0.74 |
| 3| 6202 | 46.54 | 15.76 | 0.91 |
| 4| 6340 | 55.38 | 18.76 | 1.01 |
| 5| 6374 | 61.37 | 20.70 | 1.08 |
| 6| 6577 | 69.67 | 23.49 | 1.17 |
| 7| 6681 | 77.35 | 26.02 | 1.26 |
| 8| 6919 | 95.02 | 32.11 | 1.45 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 6005 | 30.23 | 10.73 | 0.74 |
| 10 | 20 | 1139 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1710 | 6856 | 80.48 | 30.61 | 1.32 |
| 10 | 38 | 2163 | 7125 | 96.88 | 37.08 | 1.51 |

