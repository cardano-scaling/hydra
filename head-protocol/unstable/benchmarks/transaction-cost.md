--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-23 14:26:14.963455059 UTC |
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
| 1| 5465 | 9.67 | 3.04 | 0.49 |
| 2| 5564 | 10.45 | 3.28 | 0.50 |
| 3| 5657 | 11.06 | 3.46 | 0.51 |
| 5| 5849 | 12.33 | 3.84 | 0.53 |
| 10| 6329 | 15.33 | 4.73 | 0.59 |
| 50| 10175 | 42.66 | 12.81 | 1.03 |
| 100| 14972 | 76.62 | 22.83 | 1.59 |
| 114| 16316 | 86.06 | 25.59 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.47 | 6.43 | 0.42 |
| 2| 1919 | 19.76 | 7.47 | 0.45 |
| 3| 2048 | 20.85 | 8.46 | 0.47 |
| 5| 2313 | 23.10 | 10.47 | 0.51 |
| 10| 2968 | 28.77 | 15.49 | 0.62 |
| 50| 8207 | 75.58 | 55.96 | 1.53 |
| 70| 10829 | 99.05 | 76.21 | 1.98 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 721 | 18.09 | 7.03 | 0.38 |
| 3| 853 | 19.15 | 8.01 | 0.40 |
| 5| 1115 | 21.30 | 9.99 | 0.44 |
| 10| 1769 | 26.84 | 14.98 | 0.55 |
| 50| 7010 | 72.97 | 55.27 | 1.45 |
| 74| 10156 | 99.95 | 79.27 | 1.98 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 635 | 20.65 | 6.88 | 0.39 |
| 2| 737 | 23.34 | 8.32 | 0.43 |
| 3| 864 | 25.02 | 9.47 | 0.46 |
| 5| 1160 | 26.91 | 11.35 | 0.50 |
| 10| 1815 | 36.12 | 17.30 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 25.16 | 8.15 | 0.44 |
| 2| 765 | 26.86 | 9.31 | 0.47 |
| 3| 888 | 28.71 | 10.50 | 0.50 |
| 5| 1154 | 32.44 | 12.90 | 0.56 |
| 10| 1808 | 42.76 | 19.17 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 15.12 | 5.24 | 0.55 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5868 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1138 | 6207 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6546 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2272 | 6882 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7092 | 98.80 | 40.03 | 1.55 |

