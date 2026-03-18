--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-18 12:26:11.763026024 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | c8a101a5c8ac4816b0dceb59ce31fc2258e387de828f02961d2f2045 | 2652 | 
| νCommit | 61458bc2f297fff3cc5df6ac7ab57cefd87763b0b7bd722146a1035c | 685 | 
| νHead | 5788da8969b01bb1d9fd7b78b0dcd988ef2b1d4519e0deae656cef53 | 12374 | 
| μHead | d81fa4e721cac05546c901514e27fad626a1f6a8e11b4d6113d85dee* | 5284 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5837 | 10.74 | 3.42 | 0.52 |
| 2| 6035 | 12.54 | 3.97 | 0.55 |
| 3| 6239 | 14.71 | 4.65 | 0.58 |
| 5| 6641 | 18.58 | 5.86 | 0.63 |
| 10| 7647 | 29.49 | 9.31 | 0.79 |
| 43| 14279 | 98.75 | 30.86 | 1.80 |


## `Commit` transaction costs
 This uses ada-only outputs for better comparability.

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 561 | 2.44 | 1.16 | 0.20 |
| 2| 742 | 3.38 | 1.73 | 0.22 |
| 3| 919 | 4.36 | 2.33 | 0.24 |
| 5| 1280 | 6.41 | 3.60 | 0.28 |
| 10| 2180 | 12.13 | 7.25 | 0.40 |
| 54| 10058 | 98.61 | 68.52 | 1.88 |


## `CollectCom` transaction costs

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 525 | 18.29 | 5.75 | 0.36 |
| 2 | 114 | 640 | 24.74 | 7.72 | 0.43 |
| 3 | 170 | 747 | 31.41 | 9.75 | 0.51 |
| 4 | 227 | 858 | 39.02 | 11.99 | 0.59 |
| 5 | 284 | 969 | 46.97 | 14.37 | 0.68 |
| 6 | 341 | 1085 | 52.62 | 16.22 | 0.74 |
| 7 | 393 | 1192 | 63.08 | 19.14 | 0.85 |
| 8 | 450 | 1303 | 71.14 | 21.63 | 0.94 |
| 9 | 507 | 1414 | 72.45 | 22.30 | 0.96 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1798 | 17.89 | 6.25 | 0.42 |
| 2| 1926 | 18.98 | 7.24 | 0.44 |
| 3| 2149 | 21.78 | 8.74 | 0.48 |
| 5| 2401 | 23.73 | 10.67 | 0.52 |
| 10| 3103 | 29.63 | 15.77 | 0.64 |
| 50| 9237 | 89.73 | 60.27 | 1.72 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 605 | 16.65 | 5.99 | 0.35 |
| 2| 765 | 17.73 | 6.97 | 0.38 |
| 3| 898 | 18.84 | 7.97 | 0.40 |
| 5| 1252 | 22.89 | 10.51 | 0.47 |
| 10| 1932 | 28.54 | 15.51 | 0.58 |
| 50| 8004 | 84.71 | 58.87 | 1.61 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 639 | 21.91 | 7.28 | 0.41 |
| 2| 850 | 23.85 | 8.53 | 0.44 |
| 3| 921 | 24.67 | 9.43 | 0.46 |
| 5| 1211 | 25.87 | 11.14 | 0.49 |
| 10| 2023 | 33.82 | 16.88 | 0.63 |
| 49| 8099 | 96.32 | 61.50 | 1.73 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 688 | 25.37 | 8.26 | 0.45 |
| 2| 858 | 27.44 | 9.55 | 0.48 |
| 3| 1013 | 28.91 | 10.65 | 0.51 |
| 5| 1278 | 32.12 | 12.92 | 0.56 |
| 10| 2025 | 40.77 | 18.83 | 0.70 |
| 42| 6661 | 97.34 | 56.77 | 1.64 |


## `Abort` transaction costs
There is some variation due to the random mixture of initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5798 | 23.82 | 8.37 | 0.66 |
| 2| 5878 | 28.52 | 9.98 | 0.71 |
| 3| 6150 | 39.80 | 14.10 | 0.84 |
| 4| 6139 | 45.53 | 16.04 | 0.90 |
| 5| 6304 | 52.28 | 18.45 | 0.98 |
| 6| 6472 | 63.05 | 22.33 | 1.11 |
| 7| 6719 | 72.30 | 25.62 | 1.21 |
| 8| 6888 | 80.38 | 28.48 | 1.31 |
| 9| 7022 | 82.93 | 29.33 | 1.34 |
| 10| 7107 | 96.50 | 33.84 | 1.49 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5834 | 16.83 | 5.83 | 0.58 |
| 10 | 5 | 283 | 6002 | 24.61 | 9.16 | 0.68 |
| 10 | 20 | 1138 | 6512 | 52.27 | 20.64 | 1.01 |
| 10 | 30 | 1709 | 6856 | 70.91 | 28.36 | 1.23 |
| 10 | 40 | 2276 | 7192 | 90.15 | 36.29 | 1.46 |
| 10 | 45 | 2563 | 7364 | 98.69 | 39.87 | 1.56 |

