--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-28 12:06:24.942257893 UTC |
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
| 1| 5465 | 9.96 | 3.15 | 0.49 |
| 2| 5560 | 10.63 | 3.35 | 0.50 |
| 3| 5656 | 11.17 | 3.50 | 0.51 |
| 5| 5847 | 12.53 | 3.91 | 0.54 |
| 10| 6330 | 15.38 | 4.74 | 0.59 |
| 50| 10172 | 42.65 | 12.79 | 1.03 |
| 100| 14972 | 75.99 | 22.59 | 1.59 |
| 114| 16316 | 85.56 | 25.43 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 18.67 | 6.48 | 0.43 |
| 2| 1918 | 19.58 | 7.43 | 0.45 |
| 3| 2052 | 20.64 | 8.41 | 0.47 |
| 5| 2314 | 23.07 | 10.46 | 0.51 |
| 10| 2969 | 28.56 | 15.44 | 0.62 |
| 50| 8209 | 75.12 | 55.84 | 1.52 |
| 71| 10960 | 99.94 | 77.15 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.97 | 6.03 | 0.36 |
| 2| 720 | 18.09 | 7.03 | 0.38 |
| 3| 853 | 19.15 | 8.01 | 0.40 |
| 5| 1116 | 21.43 | 10.02 | 0.44 |
| 10| 1771 | 27.00 | 15.02 | 0.55 |
| 50| 7011 | 73.47 | 55.40 | 1.45 |
| 73| 10023 | 99.54 | 78.45 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 630 | 20.65 | 6.88 | 0.39 |
| 2| 766 | 22.14 | 7.98 | 0.42 |
| 3| 864 | 24.94 | 9.45 | 0.46 |
| 5| 1156 | 26.91 | 11.35 | 0.50 |
| 10| 1781 | 37.94 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.89 | 9.31 | 0.47 |
| 3| 888 | 28.68 | 10.50 | 0.50 |
| 5| 1153 | 32.42 | 12.90 | 0.56 |
| 10| 1809 | 42.84 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1140 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1709 | 6550 | 69.61 | 27.91 | 1.20 |
| 10 | 46 | 2619 | 7091 | 98.80 | 40.03 | 1.55 |

