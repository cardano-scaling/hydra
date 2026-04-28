--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-28 16:50:39.329365158 UTC |
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
| 1| 5462 | 10.00 | 3.16 | 0.49 |
| 2| 5561 | 10.69 | 3.37 | 0.51 |
| 3| 5654 | 10.94 | 3.42 | 0.51 |
| 5| 5850 | 12.56 | 3.93 | 0.54 |
| 10| 6327 | 15.51 | 4.79 | 0.59 |
| 50| 10171 | 42.48 | 12.74 | 1.03 |
| 100| 14969 | 76.23 | 22.67 | 1.59 |
| 114| 16315 | 85.53 | 25.42 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.76 | 7.47 | 0.45 |
| 3| 2050 | 20.72 | 8.43 | 0.47 |
| 5| 2310 | 23.15 | 10.48 | 0.51 |
| 10| 2968 | 28.75 | 15.48 | 0.62 |
| 50| 8209 | 74.55 | 55.69 | 1.52 |
| 71| 10961 | 99.13 | 76.94 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 724 | 18.03 | 7.01 | 0.38 |
| 3| 856 | 19.12 | 8.01 | 0.40 |
| 5| 1114 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.82 | 14.97 | 0.55 |
| 50| 7011 | 73.08 | 55.30 | 1.45 |
| 73| 10023 | 99.57 | 78.46 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 766 | 22.09 | 7.96 | 0.42 |
| 3| 860 | 24.97 | 9.45 | 0.46 |
| 5| 1122 | 28.42 | 11.78 | 0.51 |
| 10| 1781 | 37.76 | 17.77 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 757 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.71 | 10.50 | 0.50 |
| 5| 1153 | 32.36 | 12.88 | 0.56 |
| 10| 1804 | 42.71 | 19.15 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5697 | 24.14 | 9.00 | 0.66 |
| 10 | 20 | 1139 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1708 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2275 | 6885 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2616 | 7089 | 98.80 | 40.03 | 1.55 |

