--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-15 08:40:09.137739113 UTC |
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
| 1| 5464 | 9.77 | 3.08 | 0.49 |
| 2| 5561 | 10.41 | 3.27 | 0.50 |
| 3| 5656 | 11.27 | 3.54 | 0.52 |
| 5| 5850 | 12.88 | 4.04 | 0.54 |
| 10| 6327 | 15.37 | 4.74 | 0.59 |
| 50| 10169 | 42.71 | 12.81 | 1.04 |
| 100| 14971 | 76.24 | 22.68 | 1.59 |
| 114| 16315 | 86.11 | 25.62 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1785 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.76 | 7.47 | 0.45 |
| 3| 2054 | 20.92 | 8.48 | 0.47 |
| 5| 2312 | 22.95 | 10.43 | 0.51 |
| 10| 2967 | 28.46 | 15.41 | 0.62 |
| 50| 8208 | 74.26 | 55.62 | 1.51 |
| 70| 10830 | 98.57 | 76.09 | 1.97 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1119 | 21.32 | 10.00 | 0.44 |
| 10| 1770 | 26.95 | 15.01 | 0.55 |
| 50| 7012 | 72.83 | 55.24 | 1.45 |
| 73| 10024 | 98.79 | 78.26 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 898 | 23.61 | 9.07 | 0.45 |
| 5| 1160 | 26.84 | 11.33 | 0.50 |
| 10| 1815 | 35.94 | 17.26 | 0.64 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 891 | 28.63 | 10.48 | 0.50 |
| 5| 1154 | 32.44 | 12.90 | 0.56 |
| 10| 1809 | 42.66 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1137 | 6205 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1710 | 6551 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2276 | 6887 | 87.85 | 35.49 | 1.42 |
| 10 | 45 | 2558 | 7054 | 96.98 | 39.28 | 1.53 |

