--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-05-06 12:41:59.377159511 UTC |
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
| 2| 5561 | 10.30 | 3.23 | 0.50 |
| 3| 5657 | 11.06 | 3.46 | 0.51 |
| 5| 5853 | 12.62 | 3.94 | 0.54 |
| 10| 6329 | 15.49 | 4.79 | 0.59 |
| 50| 10172 | 42.53 | 12.75 | 1.03 |
| 100| 14971 | 76.36 | 22.72 | 1.59 |
| 114| 16316 | 85.39 | 25.38 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.81 | 7.48 | 0.45 |
| 3| 2050 | 20.90 | 8.48 | 0.47 |
| 5| 2313 | 23.15 | 10.48 | 0.51 |
| 10| 2966 | 28.44 | 15.40 | 0.62 |
| 50| 8208 | 75.00 | 55.81 | 1.52 |
| 71| 10963 | 99.30 | 76.98 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 720 | 18.06 | 7.02 | 0.38 |
| 3| 856 | 19.12 | 8.01 | 0.40 |
| 5| 1115 | 21.30 | 9.99 | 0.44 |
| 10| 1769 | 26.89 | 14.99 | 0.55 |
| 50| 7011 | 72.69 | 55.20 | 1.44 |
| 73| 10024 | 99.40 | 78.42 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 21.80 | 7.20 | 0.40 |
| 2| 767 | 22.12 | 7.97 | 0.42 |
| 3| 865 | 24.99 | 9.46 | 0.46 |
| 5| 1164 | 26.91 | 11.35 | 0.50 |
| 10| 1781 | 37.91 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.14 | 8.14 | 0.44 |
| 2| 756 | 26.91 | 9.32 | 0.47 |
| 3| 892 | 28.68 | 10.50 | 0.50 |
| 5| 1158 | 32.47 | 12.91 | 0.56 |
| 10| 1809 | 42.86 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 58 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1135 | 6204 | 51.38 | 20.34 | 0.98 |
| 10 | 40 | 2274 | 6884 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7091 | 98.80 | 40.03 | 1.55 |

