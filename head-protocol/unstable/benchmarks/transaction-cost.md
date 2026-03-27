--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-03-27 16:23:13.459706157 UTC |
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
| 1| 5464 | 9.98 | 3.15 | 0.49 |
| 2| 5561 | 10.41 | 3.27 | 0.50 |
| 3| 5656 | 11.52 | 3.63 | 0.52 |
| 5| 5850 | 12.25 | 3.81 | 0.53 |
| 10| 6330 | 15.50 | 4.79 | 0.59 |
| 50| 10171 | 42.58 | 12.78 | 1.03 |
| 100| 14969 | 76.07 | 22.63 | 1.59 |
| 114| 16316 | 85.67 | 25.47 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1787 | 18.67 | 6.48 | 0.43 |
| 2| 1917 | 19.79 | 7.48 | 0.45 |
| 3| 2048 | 20.92 | 8.48 | 0.47 |
| 5| 2313 | 22.92 | 10.42 | 0.51 |
| 10| 2968 | 28.31 | 15.37 | 0.62 |
| 50| 8206 | 74.44 | 55.66 | 1.51 |
| 72| 11092 | 99.94 | 77.86 | 2.01 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1119 | 21.32 | 10.00 | 0.44 |
| 10| 1769 | 26.79 | 14.97 | 0.55 |
| 50| 7011 | 72.83 | 55.24 | 1.45 |
| 73| 10025 | 98.76 | 78.25 | 1.95 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 601 | 21.82 | 7.21 | 0.41 |
| 2| 728 | 23.39 | 8.33 | 0.43 |
| 3| 898 | 23.64 | 9.08 | 0.45 |
| 5| 1122 | 28.42 | 11.78 | 0.51 |
| 10| 1815 | 35.94 | 17.26 | 0.64 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 622 | 25.16 | 8.15 | 0.44 |
| 2| 756 | 26.91 | 9.32 | 0.47 |
| 3| 888 | 28.71 | 10.50 | 0.50 |
| 5| 1150 | 32.44 | 12.90 | 0.56 |
| 10| 1804 | 42.66 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 284 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 20 | 1139 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1709 | 6549 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2278 | 6888 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2617 | 7089 | 98.80 | 40.03 | 1.55 |

