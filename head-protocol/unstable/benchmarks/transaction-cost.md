--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-02-19 15:55:41.389694606 UTC |
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
| 1| 5840 | 10.69 | 3.40 | 0.52 |
| 2| 6037 | 12.80 | 4.07 | 0.55 |
| 3| 6236 | 14.31 | 4.52 | 0.57 |
| 5| 6646 | 18.62 | 5.87 | 0.64 |
| 10| 7646 | 29.18 | 9.20 | 0.79 |
| 43| 14279 | 98.64 | 30.82 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1276 | 6.41 | 3.60 | 0.28 |
| 10| 2174 | 12.13 | 7.25 | 0.40 |
| 54| 10064 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 24.42 | 7.12 | 0.42 |
| 2 | 113 | 636 | 32.27 | 9.39 | 0.51 |
| 3 | 171 | 747 | 41.38 | 11.97 | 0.60 |
| 4 | 227 | 858 | 54.08 | 15.42 | 0.74 |
| 5 | 283 | 969 | 62.72 | 17.89 | 0.83 |
| 6 | 337 | 1081 | 74.83 | 21.15 | 0.95 |
| 7 | 394 | 1192 | 83.42 | 23.69 | 1.05 |
| 8 | 449 | 1303 | 86.99 | 24.80 | 1.09 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1805 | 24.00 | 7.62 | 0.48 |
| 2| 1924 | 25.85 | 8.78 | 0.51 |
| 3| 2130 | 27.93 | 10.05 | 0.54 |
| 5| 2276 | 29.18 | 11.72 | 0.57 |
| 10| 3077 | 40.04 | 18.08 | 0.74 |
| 38| 7178 | 91.98 | 51.20 | 1.58 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 616 | 22.53 | 7.30 | 0.41 |
| 2| 795 | 25.16 | 8.70 | 0.45 |
| 3| 900 | 25.79 | 9.53 | 0.47 |
| 5| 1235 | 29.82 | 11.99 | 0.53 |
| 10| 1956 | 38.60 | 17.77 | 0.68 |
| 41| 6612 | 97.03 | 54.67 | 1.62 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 656 | 29.13 | 8.90 | 0.48 |
| 2| 732 | 30.20 | 9.84 | 0.50 |
| 3| 910 | 32.76 | 11.24 | 0.54 |
| 5| 1168 | 33.55 | 12.80 | 0.56 |
| 10| 2125 | 46.18 | 19.76 | 0.76 |
| 36| 6073 | 98.55 | 51.82 | 1.59 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 33.83 | 10.16 | 0.53 |
| 2| 881 | 36.60 | 11.61 | 0.57 |
| 3| 933 | 37.95 | 12.63 | 0.59 |
| 5| 1326 | 43.32 | 15.48 | 0.67 |
| 10| 2029 | 53.95 | 21.78 | 0.83 |
| 29| 4973 | 99.74 | 47.20 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5786 | 27.13 | 9.11 | 0.69 |
| 2| 5952 | 35.79 | 12.03 | 0.79 |
| 3| 6152 | 45.81 | 15.44 | 0.90 |
| 4| 6184 | 51.40 | 17.28 | 0.96 |
| 5| 6360 | 60.56 | 20.35 | 1.07 |
| 6| 6525 | 70.10 | 23.60 | 1.17 |
| 7| 6793 | 81.54 | 27.47 | 1.31 |
| 8| 6866 | 88.66 | 29.76 | 1.38 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5869 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6175 | 40.58 | 14.82 | 0.86 |
| 10 | 20 | 1140 | 6515 | 58.66 | 22.07 | 1.07 |
| 10 | 30 | 1706 | 6852 | 80.04 | 30.46 | 1.32 |
| 10 | 39 | 2221 | 7160 | 98.93 | 37.88 | 1.54 |

