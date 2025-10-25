--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-10-25 04:40:51.290703717 UTC |
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
| 2| 6037 | 12.25 | 3.87 | 0.54 |
| 3| 6238 | 14.48 | 4.58 | 0.57 |
| 5| 6640 | 19.02 | 6.02 | 0.64 |
| 10| 7644 | 29.30 | 9.24 | 0.79 |
| 43| 14282 | 98.58 | 30.79 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 558 | 2.44 | 1.16 | 0.20 |
| 2| 739 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1278 | 6.41 | 3.60 | 0.28 |
| 10| 2177 | 12.13 | 7.25 | 0.40 |
| 54| 10044 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 25.20 | 7.30 | 0.43 |
| 2 | 114 | 640 | 34.20 | 9.84 | 0.53 |
| 3 | 170 | 751 | 43.63 | 12.50 | 0.63 |
| 4 | 226 | 858 | 50.00 | 14.47 | 0.70 |
| 5 | 283 | 974 | 56.41 | 16.41 | 0.77 |
| 6 | 337 | 1081 | 64.37 | 18.65 | 0.85 |
| 7 | 393 | 1196 | 84.10 | 23.72 | 1.05 |
| 8 | 449 | 1307 | 95.83 | 26.96 | 1.18 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1793 | 24.00 | 7.62 | 0.48 |
| 2| 1922 | 25.88 | 8.79 | 0.51 |
| 3| 2171 | 29.18 | 10.40 | 0.56 |
| 5| 2438 | 32.12 | 12.55 | 0.61 |
| 10| 3242 | 42.72 | 18.84 | 0.77 |
| 40| 7539 | 97.38 | 54.00 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 22.57 | 7.31 | 0.41 |
| 2| 751 | 24.35 | 8.47 | 0.44 |
| 3| 899 | 25.10 | 9.32 | 0.46 |
| 5| 1221 | 29.10 | 11.77 | 0.52 |
| 10| 1965 | 37.70 | 17.51 | 0.67 |
| 41| 6504 | 95.63 | 54.31 | 1.60 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 657 | 29.17 | 8.91 | 0.48 |
| 2| 855 | 29.89 | 9.82 | 0.50 |
| 3| 926 | 32.76 | 11.24 | 0.54 |
| 5| 1277 | 37.58 | 13.95 | 0.61 |
| 10| 1893 | 46.01 | 19.61 | 0.75 |
| 37| 5771 | 94.96 | 51.33 | 1.54 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 33.15 | 9.95 | 0.52 |
| 2| 813 | 35.85 | 11.38 | 0.56 |
| 3| 1059 | 39.34 | 13.05 | 0.61 |
| 5| 1202 | 41.82 | 15.03 | 0.65 |
| 10| 1960 | 53.50 | 21.63 | 0.82 |
| 29| 4775 | 97.88 | 46.64 | 1.49 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5811 | 27.09 | 9.08 | 0.69 |
| 2| 6013 | 36.98 | 12.47 | 0.80 |
| 3| 6155 | 45.81 | 15.45 | 0.90 |
| 4| 6341 | 57.22 | 19.37 | 1.03 |
| 5| 6440 | 65.11 | 22.02 | 1.12 |
| 6| 6429 | 68.73 | 23.01 | 1.15 |
| 7| 6768 | 81.90 | 27.59 | 1.31 |
| 8| 6733 | 83.96 | 28.15 | 1.33 |
| 9| 6755 | 89.54 | 29.94 | 1.39 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 17.86 | 5.96 | 0.59 |
| 10 | 1 | 57 | 5869 | 20.34 | 6.91 | 0.62 |
| 10 | 20 | 1138 | 6512 | 59.98 | 22.53 | 1.08 |
| 10 | 30 | 1708 | 6855 | 80.04 | 30.46 | 1.32 |
| 10 | 37 | 2106 | 7092 | 94.83 | 36.27 | 1.49 |

