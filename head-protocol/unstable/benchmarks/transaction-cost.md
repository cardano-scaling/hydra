--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-21 12:38:53.107091533 UTC |
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
| 1| 5465 | 9.76 | 3.07 | 0.49 |
| 2| 5561 | 10.64 | 3.35 | 0.51 |
| 3| 5657 | 11.03 | 3.45 | 0.51 |
| 5| 5850 | 12.35 | 3.85 | 0.54 |
| 10| 6329 | 15.87 | 4.92 | 0.59 |
| 50| 10169 | 42.81 | 12.86 | 1.04 |
| 100| 14971 | 76.38 | 22.73 | 1.59 |
| 114| 16313 | 86.00 | 25.59 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.81 | 7.48 | 0.45 |
| 3| 2050 | 20.64 | 8.41 | 0.47 |
| 5| 2312 | 22.95 | 10.43 | 0.51 |
| 10| 2972 | 28.46 | 15.41 | 0.62 |
| 50| 8207 | 75.39 | 55.92 | 1.52 |
| 72| 11091 | 99.99 | 77.87 | 2.01 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.06 | 7.02 | 0.38 |
| 3| 851 | 19.12 | 8.01 | 0.40 |
| 5| 1115 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.97 | 15.01 | 0.55 |
| 50| 7011 | 73.25 | 55.35 | 1.45 |
| 73| 10024 | 99.71 | 78.50 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 732 | 23.39 | 8.33 | 0.43 |
| 3| 864 | 24.94 | 9.45 | 0.46 |
| 5| 1126 | 28.45 | 11.78 | 0.51 |
| 10| 1781 | 37.94 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.86 | 9.31 | 0.47 |
| 3| 888 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.34 | 12.88 | 0.56 |
| 10| 1809 | 42.68 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 10 | 569 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 40 | 2276 | 6886 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2618 | 7090 | 98.80 | 40.03 | 1.55 |

