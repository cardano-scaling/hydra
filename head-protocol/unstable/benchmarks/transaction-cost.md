--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-20 08:02:46.662507878 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 21fa6ee40ea957042a3dee8bae69b15c7ed88102be7ecb056d8911b2 | 11452 | 
| μHead | aac60f1ab5b37ad38b49163a7a13bcbbdcb8ab12d839720cd7d8748e* | 4978 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5465 | 9.98 | 3.15 | 0.49 |
| 2| 5558 | 10.33 | 3.24 | 0.50 |
| 3| 5657 | 10.83 | 3.38 | 0.51 |
| 5| 5849 | 12.35 | 3.85 | 0.54 |
| 10| 6329 | 15.58 | 4.81 | 0.59 |
| 50| 10169 | 42.31 | 12.67 | 1.03 |
| 100| 14973 | 76.39 | 22.75 | 1.59 |
| 114| 16315 | 85.50 | 25.40 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1786 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.58 | 7.43 | 0.45 |
| 3| 2048 | 20.87 | 8.47 | 0.47 |
| 5| 2317 | 22.97 | 10.43 | 0.51 |
| 10| 2967 | 28.46 | 15.41 | 0.62 |
| 50| 8207 | 74.80 | 55.76 | 1.52 |
| 71| 10960 | 98.94 | 76.89 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.97 | 6.03 | 0.36 |
| 2| 720 | 18.06 | 7.02 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1119 | 21.38 | 10.01 | 0.44 |
| 10| 1770 | 26.77 | 14.96 | 0.55 |
| 50| 7011 | 73.41 | 55.39 | 1.45 |
| 73| 10024 | 99.65 | 78.48 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 21.82 | 7.21 | 0.41 |
| 2| 732 | 23.34 | 8.32 | 0.43 |
| 3| 898 | 23.64 | 9.08 | 0.45 |
| 5| 1161 | 26.84 | 11.33 | 0.50 |
| 10| 1816 | 36.07 | 17.29 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 757 | 26.91 | 9.32 | 0.47 |
| 3| 891 | 28.68 | 10.50 | 0.50 |
| 5| 1153 | 32.44 | 12.90 | 0.56 |
| 10| 1809 | 42.74 | 19.16 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5697 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2281 | 6891 | 87.85 | 35.49 | 1.42 |
| 10 | 45 | 2562 | 7058 | 96.98 | 39.28 | 1.53 |

