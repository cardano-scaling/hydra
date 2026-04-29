--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-29 19:11:31.086536619 UTC |
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
| 1| 5465 | 10.03 | 3.17 | 0.49 |
| 2| 5562 | 10.41 | 3.27 | 0.50 |
| 3| 5657 | 11.29 | 3.54 | 0.52 |
| 5| 5849 | 12.34 | 3.84 | 0.54 |
| 10| 6330 | 15.33 | 4.73 | 0.59 |
| 50| 10171 | 42.18 | 12.64 | 1.03 |
| 100| 14969 | 76.10 | 22.62 | 1.59 |
| 114| 16313 | 85.20 | 25.31 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.47 | 6.43 | 0.42 |
| 2| 1917 | 19.79 | 7.48 | 0.45 |
| 3| 2050 | 20.92 | 8.48 | 0.47 |
| 5| 2318 | 22.92 | 10.42 | 0.51 |
| 10| 2966 | 28.54 | 15.43 | 0.62 |
| 50| 8206 | 75.48 | 55.94 | 1.52 |
| 71| 10960 | 98.22 | 76.71 | 1.98 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.95 | 6.02 | 0.36 |
| 2| 722 | 18.09 | 7.03 | 0.38 |
| 3| 853 | 19.20 | 8.03 | 0.40 |
| 5| 1116 | 21.43 | 10.02 | 0.44 |
| 10| 1769 | 26.89 | 14.99 | 0.55 |
| 50| 7011 | 73.58 | 55.43 | 1.45 |
| 73| 10024 | 99.54 | 78.45 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 631 | 20.65 | 6.88 | 0.39 |
| 2| 736 | 23.39 | 8.33 | 0.43 |
| 3| 899 | 23.61 | 9.07 | 0.45 |
| 5| 1126 | 28.32 | 11.75 | 0.51 |
| 10| 1781 | 37.96 | 17.82 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.16 | 8.15 | 0.44 |
| 2| 764 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.63 | 10.48 | 0.50 |
| 5| 1154 | 32.39 | 12.89 | 0.56 |
| 10| 1808 | 42.71 | 19.15 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1140 | 6209 | 51.38 | 20.34 | 0.99 |
| 10 | 40 | 2274 | 6884 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7092 | 98.80 | 40.03 | 1.55 |

