--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-28 19:32:56.491118953 UTC |
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
| 1| 5836 | 10.40 | 3.30 | 0.51 |
| 2| 6035 | 12.34 | 3.90 | 0.54 |
| 3| 6239 | 14.50 | 4.58 | 0.58 |
| 5| 6640 | 18.64 | 5.88 | 0.64 |
| 10| 7651 | 28.81 | 9.07 | 0.79 |
| 43| 14281 | 98.56 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 743 | 3.38 | 1.73 | 0.22 |
| 3| 918 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2170 | 12.13 | 7.25 | 0.40 |
| 54| 10076 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 636 | 33.25 | 9.63 | 0.52 |
| 3 | 169 | 747 | 42.57 | 12.25 | 0.62 |
| 4 | 227 | 858 | 48.09 | 13.97 | 0.68 |
| 5 | 283 | 969 | 56.35 | 16.36 | 0.77 |
| 6 | 340 | 1081 | 64.69 | 18.76 | 0.86 |
| 7 | 396 | 1192 | 80.83 | 23.07 | 1.02 |
| 8 | 452 | 1303 | 96.51 | 27.28 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 24.29 | 7.69 | 0.48 |
| 2| 1967 | 26.91 | 9.08 | 0.52 |
| 3| 2074 | 27.40 | 9.88 | 0.53 |
| 5| 2487 | 33.12 | 12.83 | 0.62 |
| 10| 3116 | 40.85 | 18.32 | 0.75 |
| 38| 7451 | 95.72 | 52.23 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 650 | 22.81 | 7.37 | 0.42 |
| 2| 696 | 22.62 | 7.97 | 0.42 |
| 3| 914 | 27.10 | 9.89 | 0.48 |
| 5| 1226 | 29.91 | 12.02 | 0.53 |
| 10| 1932 | 37.40 | 17.43 | 0.66 |
| 41| 6748 | 99.97 | 55.50 | 1.65 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 655 | 29.17 | 8.91 | 0.48 |
| 2| 775 | 30.98 | 10.08 | 0.51 |
| 3| 953 | 30.98 | 10.76 | 0.52 |
| 5| 1330 | 35.38 | 13.35 | 0.59 |
| 10| 1929 | 43.70 | 18.99 | 0.73 |
| 35| 5943 | 96.78 | 50.66 | 1.56 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 667 | 33.83 | 10.15 | 0.53 |
| 2| 765 | 35.14 | 11.16 | 0.55 |
| 3| 948 | 37.84 | 12.60 | 0.59 |
| 5| 1260 | 42.68 | 15.29 | 0.66 |
| 10| 2201 | 56.19 | 22.46 | 0.86 |
| 28| 4964 | 99.76 | 46.62 | 1.51 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5842 | 27.05 | 9.08 | 0.69 |
| 2| 5823 | 31.56 | 10.51 | 0.74 |
| 3| 6085 | 44.86 | 15.09 | 0.89 |
| 4| 6298 | 55.14 | 18.57 | 1.01 |
| 5| 6414 | 61.70 | 20.76 | 1.08 |
| 6| 6560 | 74.35 | 25.07 | 1.22 |
| 7| 6515 | 70.80 | 23.68 | 1.18 |
| 8| 6675 | 84.19 | 28.21 | 1.33 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6005 | 28.90 | 10.28 | 0.72 |
| 10 | 10 | 567 | 6171 | 39.95 | 14.60 | 0.85 |
| 10 | 20 | 1135 | 6510 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1709 | 6856 | 79.78 | 30.37 | 1.32 |
| 10 | 40 | 2279 | 7196 | 99.66 | 38.24 | 1.55 |

