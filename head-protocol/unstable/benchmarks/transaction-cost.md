--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-14 09:15:41.483405299 UTC |
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
| 1| 5469 | 9.59 | 3.01 | 0.49 |
| 2| 5565 | 10.22 | 3.20 | 0.50 |
| 3| 5657 | 10.96 | 3.43 | 0.51 |
| 5| 5849 | 12.35 | 3.85 | 0.54 |
| 10| 6330 | 15.92 | 4.93 | 0.59 |
| 50| 10172 | 42.64 | 12.80 | 1.03 |
| 100| 14971 | 76.00 | 22.61 | 1.59 |
| 114| 16316 | 86.02 | 25.56 | 1.75 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.61 | 7.43 | 0.45 |
| 3| 2055 | 20.85 | 8.46 | 0.47 |
| 5| 2311 | 23.02 | 10.45 | 0.51 |
| 10| 2965 | 28.72 | 15.48 | 0.62 |
| 50| 8209 | 74.50 | 55.68 | 1.51 |
| 72| 11090 | 99.44 | 77.73 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 726 | 18.06 | 7.02 | 0.38 |
| 3| 853 | 19.20 | 8.03 | 0.40 |
| 5| 1119 | 21.43 | 10.02 | 0.44 |
| 10| 1770 | 26.95 | 15.01 | 0.55 |
| 50| 7010 | 73.50 | 55.41 | 1.45 |
| 74| 10155 | 99.82 | 79.23 | 1.97 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 20.62 | 6.87 | 0.39 |
| 2| 736 | 23.39 | 8.33 | 0.43 |
| 3| 898 | 23.61 | 9.07 | 0.45 |
| 5| 1160 | 26.97 | 11.37 | 0.50 |
| 10| 1811 | 36.02 | 17.28 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 633 | 25.14 | 8.14 | 0.44 |
| 2| 765 | 26.86 | 9.31 | 0.47 |
| 3| 892 | 28.63 | 10.48 | 0.50 |
| 5| 1153 | 32.47 | 12.91 | 0.56 |
| 10| 1808 | 42.66 | 19.14 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 5 | 285 | 5699 | 24.14 | 9.00 | 0.66 |
| 10 | 10 | 570 | 5869 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1141 | 6209 | 51.38 | 20.34 | 0.99 |
| 10 | 44 | 2506 | 7025 | 95.15 | 38.52 | 1.51 |

