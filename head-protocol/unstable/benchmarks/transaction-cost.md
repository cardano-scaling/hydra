--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 08:02:41.761905548 UTC |
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
| 1| 5465 | 9.77 | 3.08 | 0.49 |
| 2| 5561 | 10.78 | 3.40 | 0.51 |
| 3| 5656 | 11.50 | 3.62 | 0.52 |
| 5| 5849 | 12.47 | 3.90 | 0.54 |
| 10| 6329 | 15.56 | 4.80 | 0.59 |
| 50| 10171 | 42.38 | 12.71 | 1.03 |
| 100| 14972 | 76.40 | 22.74 | 1.59 |
| 114| 16315 | 85.40 | 25.37 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1789 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.76 | 7.47 | 0.45 |
| 3| 2049 | 20.90 | 8.48 | 0.47 |
| 5| 2314 | 23.13 | 10.47 | 0.51 |
| 10| 2972 | 28.51 | 15.42 | 0.62 |
| 50| 8209 | 74.90 | 55.78 | 1.52 |
| 71| 10958 | 99.34 | 77.00 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.20 | 8.03 | 0.40 |
| 5| 1115 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.74 | 14.95 | 0.55 |
| 50| 7015 | 73.41 | 55.39 | 1.45 |
| 73| 10025 | 98.98 | 78.31 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 602 | 21.82 | 7.21 | 0.41 |
| 2| 762 | 22.12 | 7.97 | 0.42 |
| 3| 864 | 24.99 | 9.46 | 0.46 |
| 5| 1160 | 26.89 | 11.34 | 0.50 |
| 10| 1811 | 35.94 | 17.26 | 0.64 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 25.16 | 8.15 | 0.44 |
| 2| 761 | 26.89 | 9.31 | 0.47 |
| 3| 891 | 28.68 | 10.50 | 0.50 |
| 5| 1154 | 32.42 | 12.90 | 0.56 |
| 10| 1808 | 42.81 | 19.18 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1140 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1707 | 6548 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2278 | 6888 | 87.85 | 35.49 | 1.42 |
| 10 | 45 | 2563 | 7059 | 96.98 | 39.28 | 1.53 |

