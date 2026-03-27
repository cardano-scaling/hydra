--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-27 16:39:39.321684292 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 71973234989552e58bd91b0b78241260c51102b474bfa8fc6d730f45 | 11452 | 
| μHead | 0b32eaac26cf8392f665b0b14e6f7b95bdbc4b8b5bac81b1ef7241a1* | 4978 | 
| νDeposit | ae01dade3a9c346d5c93ae3ce339412b90a0b8f83f94ec6baa24e30c | 1102 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5462 | 9.58 | 3.01 | 0.49 |
| 2| 5562 | 10.68 | 3.37 | 0.51 |
| 3| 5657 | 11.27 | 3.54 | 0.52 |
| 5| 5853 | 12.14 | 3.77 | 0.53 |
| 10| 6329 | 15.51 | 4.79 | 0.59 |
| 50| 10169 | 42.87 | 12.88 | 1.04 |
| 100| 14969 | 76.14 | 22.66 | 1.59 |
| 114| 16316 | 85.51 | 25.41 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1792 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.81 | 7.48 | 0.45 |
| 3| 2050 | 20.70 | 8.42 | 0.47 |
| 5| 2313 | 23.13 | 10.47 | 0.51 |
| 10| 2967 | 28.61 | 15.45 | 0.62 |
| 50| 8209 | 74.99 | 55.81 | 1.52 |
| 71| 10959 | 99.76 | 77.11 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1119 | 21.40 | 10.02 | 0.44 |
| 10| 1775 | 26.95 | 15.01 | 0.55 |
| 50| 7011 | 73.64 | 55.45 | 1.45 |
| 73| 10024 | 99.90 | 78.55 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 736 | 23.37 | 8.32 | 0.43 |
| 3| 894 | 23.67 | 9.08 | 0.45 |
| 5| 1164 | 26.86 | 11.34 | 0.50 |
| 10| 1816 | 36.07 | 17.29 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 764 | 26.91 | 9.32 | 0.47 |
| 3| 892 | 28.71 | 10.50 | 0.50 |
| 5| 1158 | 32.34 | 12.88 | 0.56 |
| 10| 1808 | 42.79 | 19.17 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 568 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2279 | 6890 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2613 | 7085 | 98.80 | 40.03 | 1.55 |

