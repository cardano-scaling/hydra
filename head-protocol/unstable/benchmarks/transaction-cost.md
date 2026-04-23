--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-23 15:10:55.265710375 UTC |
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
| 1| 5464 | 9.86 | 3.11 | 0.49 |
| 2| 5564 | 10.45 | 3.28 | 0.50 |
| 3| 5656 | 11.08 | 3.47 | 0.51 |
| 5| 5849 | 12.16 | 3.78 | 0.53 |
| 10| 6331 | 15.42 | 4.76 | 0.59 |
| 50| 10172 | 42.75 | 12.82 | 1.04 |
| 100| 14971 | 76.59 | 22.79 | 1.59 |
| 114| 16320 | 86.41 | 25.69 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.76 | 7.47 | 0.45 |
| 3| 2053 | 20.74 | 8.44 | 0.47 |
| 5| 2311 | 23.13 | 10.47 | 0.51 |
| 10| 2969 | 28.36 | 15.38 | 0.62 |
| 50| 8209 | 74.91 | 55.79 | 1.52 |
| 72| 11092 | 99.60 | 77.77 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.06 | 7.02 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1119 | 21.43 | 10.02 | 0.44 |
| 10| 1771 | 26.74 | 14.95 | 0.55 |
| 50| 7009 | 73.08 | 55.30 | 1.45 |
| 73| 10023 | 98.93 | 78.29 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 736 | 23.37 | 8.32 | 0.43 |
| 3| 898 | 23.69 | 9.09 | 0.45 |
| 5| 1126 | 28.32 | 11.75 | 0.51 |
| 10| 1820 | 35.92 | 17.25 | 0.64 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 764 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.71 | 10.50 | 0.50 |
| 5| 1149 | 32.42 | 12.90 | 0.56 |
| 10| 1812 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1139 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6546 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2282 | 6893 | 87.85 | 35.49 | 1.42 |
| 10 | 44 | 2503 | 7021 | 95.15 | 38.52 | 1.51 |

