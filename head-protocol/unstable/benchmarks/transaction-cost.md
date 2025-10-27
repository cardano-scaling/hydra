--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-27 15:02:13.726253662 UTC |
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
| 2| 6037 | 12.63 | 4.00 | 0.55 |
| 3| 6236 | 14.71 | 4.65 | 0.58 |
| 5| 6643 | 18.72 | 5.91 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 740 | 3.38 | 1.73 | 0.22 |
| 3| 923 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2179 | 12.13 | 7.25 | 0.40 |
| 54| 10074 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 640 | 34.27 | 9.87 | 0.53 |
| 3 | 170 | 751 | 41.00 | 11.85 | 0.60 |
| 4 | 227 | 858 | 49.67 | 14.37 | 0.69 |
| 5 | 282 | 969 | 60.91 | 17.42 | 0.81 |
| 6 | 341 | 1081 | 74.90 | 21.17 | 0.96 |
| 7 | 395 | 1192 | 80.86 | 22.99 | 1.02 |
| 8 | 448 | 1303 | 93.42 | 26.38 | 1.15 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 23.92 | 7.60 | 0.48 |
| 2| 1916 | 25.39 | 8.68 | 0.50 |
| 3| 2074 | 27.32 | 9.86 | 0.53 |
| 5| 2416 | 32.60 | 12.67 | 0.61 |
| 10| 2995 | 37.86 | 17.49 | 0.71 |
| 38| 7412 | 95.81 | 52.30 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 643 | 22.50 | 7.30 | 0.41 |
| 2| 808 | 25.16 | 8.70 | 0.45 |
| 3| 989 | 28.15 | 10.20 | 0.49 |
| 5| 1380 | 32.88 | 12.85 | 0.57 |
| 10| 2086 | 41.71 | 18.65 | 0.71 |
| 42| 6685 | 96.55 | 55.24 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 660 | 29.09 | 8.89 | 0.48 |
| 2| 858 | 29.97 | 9.84 | 0.50 |
| 3| 944 | 30.94 | 10.75 | 0.52 |
| 5| 1329 | 35.68 | 13.45 | 0.59 |
| 10| 2131 | 46.44 | 19.83 | 0.76 |
| 36| 6063 | 97.56 | 51.51 | 1.58 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.83 | 10.16 | 0.53 |
| 2| 765 | 35.17 | 11.17 | 0.55 |
| 3| 940 | 37.88 | 12.61 | 0.59 |
| 5| 1288 | 43.32 | 15.48 | 0.67 |
| 10| 2032 | 54.21 | 21.85 | 0.84 |
| 31| 4923 | 99.92 | 48.51 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5783 | 27.16 | 9.10 | 0.69 |
| 2| 5889 | 32.64 | 10.90 | 0.75 |
| 3| 6001 | 41.29 | 13.80 | 0.85 |
| 4| 6237 | 51.48 | 17.31 | 0.97 |
| 5| 6478 | 64.98 | 21.92 | 1.12 |
| 6| 6394 | 64.85 | 21.68 | 1.11 |
| 7| 6683 | 81.53 | 27.36 | 1.30 |
| 8| 6756 | 84.17 | 28.20 | 1.33 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 5 | 285 | 6004 | 29.35 | 10.43 | 0.73 |
| 10 | 10 | 568 | 6173 | 40.39 | 14.75 | 0.85 |
| 10 | 20 | 1140 | 6514 | 59.10 | 22.22 | 1.07 |
| 10 | 30 | 1710 | 6856 | 79.60 | 30.31 | 1.31 |
| 10 | 39 | 2221 | 7161 | 98.49 | 37.73 | 1.53 |

