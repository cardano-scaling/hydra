--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 12:36:18.245408173 UTC |
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
| 1| 5464 | 10.17 | 3.22 | 0.50 |
| 2| 5560 | 10.33 | 3.24 | 0.50 |
| 3| 5657 | 11.27 | 3.54 | 0.52 |
| 5| 5850 | 12.55 | 3.92 | 0.54 |
| 10| 6327 | 15.56 | 4.80 | 0.59 |
| 50| 10169 | 42.43 | 12.72 | 1.03 |
| 100| 14971 | 76.69 | 22.84 | 1.59 |
| 114| 16315 | 85.69 | 25.44 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 18.47 | 6.43 | 0.42 |
| 2| 1920 | 19.58 | 7.43 | 0.45 |
| 3| 2050 | 20.87 | 8.47 | 0.47 |
| 5| 2313 | 22.87 | 10.41 | 0.51 |
| 10| 2967 | 28.59 | 15.44 | 0.62 |
| 50| 8209 | 75.25 | 55.88 | 1.52 |
| 71| 10960 | 98.86 | 76.87 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 591 | 16.97 | 6.03 | 0.36 |
| 2| 722 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.15 | 8.01 | 0.40 |
| 5| 1115 | 21.40 | 10.02 | 0.44 |
| 10| 1769 | 26.87 | 14.99 | 0.55 |
| 50| 7010 | 72.69 | 55.20 | 1.44 |
| 73| 10024 | 98.50 | 78.18 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 762 | 22.09 | 7.96 | 0.42 |
| 3| 898 | 23.69 | 9.09 | 0.45 |
| 5| 1160 | 26.97 | 11.37 | 0.50 |
| 10| 1778 | 37.91 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 25.16 | 8.15 | 0.44 |
| 2| 760 | 26.89 | 9.31 | 0.47 |
| 3| 892 | 28.71 | 10.50 | 0.50 |
| 5| 1150 | 32.47 | 12.91 | 0.56 |
| 10| 1808 | 42.61 | 19.12 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 56 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 568 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6206 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6546 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2275 | 6886 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2617 | 7090 | 98.80 | 40.03 | 1.55 |

