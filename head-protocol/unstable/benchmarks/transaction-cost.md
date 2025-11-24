--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2025-11-24 04:58:14.154657221 UTC |
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
| 1| 5837 | 10.55 | 3.35 | 0.52 |
| 2| 6037 | 12.63 | 4.00 | 0.55 |
| 3| 6240 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 19.19 | 6.08 | 0.64 |
| 10| 7647 | 28.71 | 9.03 | 0.78 |
| 43| 14279 | 99.04 | 30.96 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 738 | 3.38 | 1.73 | 0.22 |
| 3| 920 | 4.36 | 2.33 | 0.24 |
| 5| 1277 | 6.41 | 3.60 | 0.28 |
| 10| 2181 | 12.13 | 7.25 | 0.40 |
| 54| 10045 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 529 | 25.24 | 7.32 | 0.43 |
| 2 | 114 | 640 | 33.25 | 9.63 | 0.52 |
| 3 | 169 | 747 | 40.05 | 11.65 | 0.59 |
| 4 | 226 | 858 | 51.05 | 14.67 | 0.71 |
| 5 | 283 | 974 | 56.42 | 16.41 | 0.77 |
| 6 | 339 | 1081 | 72.37 | 20.68 | 0.93 |
| 7 | 393 | 1192 | 84.87 | 23.99 | 1.06 |
| 8 | 451 | 1307 | 81.04 | 23.53 | 1.03 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 23.92 | 7.60 | 0.48 |
| 2| 1932 | 25.85 | 8.78 | 0.51 |
| 3| 2162 | 29.02 | 10.36 | 0.55 |
| 5| 2382 | 31.05 | 12.25 | 0.59 |
| 10| 3133 | 40.05 | 18.10 | 0.74 |
| 38| 7463 | 96.30 | 52.40 | 1.63 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 600 | 22.84 | 7.39 | 0.41 |
| 2| 738 | 24.00 | 8.37 | 0.44 |
| 3| 894 | 25.13 | 9.34 | 0.46 |
| 5| 1136 | 28.15 | 11.52 | 0.51 |
| 10| 2095 | 40.47 | 18.29 | 0.70 |
| 40| 6580 | 98.93 | 54.54 | 1.63 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 695 | 27.50 | 8.46 | 0.46 |
| 2| 736 | 30.20 | 9.84 | 0.50 |
| 3| 980 | 30.86 | 10.73 | 0.52 |
| 5| 1268 | 35.05 | 13.25 | 0.58 |
| 10| 1986 | 43.96 | 19.09 | 0.73 |
| 38| 6184 | 99.16 | 53.23 | 1.61 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 675 | 33.87 | 10.16 | 0.53 |
| 2| 836 | 35.85 | 11.38 | 0.56 |
| 3| 899 | 37.20 | 12.40 | 0.58 |
| 5| 1220 | 41.97 | 15.07 | 0.65 |
| 10| 1986 | 53.12 | 21.53 | 0.82 |
| 30| 4935 | 99.62 | 47.77 | 1.52 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5782 | 27.04 | 9.09 | 0.69 |
| 2| 6047 | 36.95 | 12.46 | 0.80 |
| 3| 6015 | 41.56 | 13.89 | 0.85 |
| 4| 6258 | 53.76 | 18.07 | 0.99 |
| 5| 6479 | 64.00 | 21.60 | 1.11 |
| 6| 6576 | 71.54 | 24.07 | 1.19 |
| 7| 6810 | 85.72 | 28.97 | 1.35 |
| 8| 6851 | 93.28 | 31.42 | 1.43 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5835 | 19.19 | 6.41 | 0.61 |
| 10 | 1 | 57 | 5868 | 21.22 | 7.21 | 0.63 |
| 10 | 10 | 570 | 6175 | 39.51 | 14.45 | 0.85 |
| 10 | 30 | 1709 | 6855 | 81.37 | 30.91 | 1.33 |
| 10 | 39 | 2218 | 7157 | 98.49 | 37.73 | 1.53 |

