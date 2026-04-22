--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-22 09:52:53.756542653 UTC |
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
| 1| 5464 | 10.15 | 3.21 | 0.50 |
| 2| 5560 | 10.64 | 3.35 | 0.51 |
| 3| 5658 | 11.34 | 3.56 | 0.52 |
| 5| 5847 | 12.33 | 3.84 | 0.53 |
| 10| 6327 | 15.76 | 4.88 | 0.59 |
| 50| 10171 | 42.53 | 12.75 | 1.03 |
| 100| 14969 | 76.16 | 22.67 | 1.59 |
| 114| 16313 | 85.89 | 25.54 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.76 | 7.47 | 0.45 |
| 3| 2050 | 20.74 | 8.44 | 0.47 |
| 5| 2311 | 22.92 | 10.42 | 0.51 |
| 10| 2968 | 28.69 | 15.47 | 0.62 |
| 50| 8207 | 74.50 | 55.68 | 1.51 |
| 71| 10961 | 98.40 | 76.75 | 1.98 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 589 | 16.95 | 6.02 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1116 | 21.33 | 10.00 | 0.44 |
| 10| 1768 | 26.89 | 14.99 | 0.55 |
| 50| 7012 | 73.05 | 55.30 | 1.45 |
| 73| 10023 | 99.23 | 78.37 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 636 | 20.65 | 6.88 | 0.39 |
| 2| 766 | 22.09 | 7.96 | 0.42 |
| 3| 902 | 23.67 | 9.08 | 0.45 |
| 5| 1160 | 26.91 | 11.35 | 0.50 |
| 10| 1815 | 36.15 | 17.31 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 896 | 28.71 | 10.50 | 0.50 |
| 5| 1149 | 32.42 | 12.90 | 0.56 |
| 10| 1809 | 42.71 | 19.15 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2274 | 6884 | 87.85 | 35.49 | 1.42 |
| 10 | 44 | 2504 | 7023 | 95.15 | 38.52 | 1.51 |

