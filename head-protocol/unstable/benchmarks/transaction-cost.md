--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-04-02 11:01:47.477222192 UTC |
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
| 1| 5462 | 9.90 | 3.12 | 0.49 |
| 2| 5558 | 10.87 | 3.43 | 0.51 |
| 3| 5654 | 11.38 | 3.58 | 0.52 |
| 5| 5854 | 12.12 | 3.77 | 0.53 |
| 10| 6333 | 15.37 | 4.74 | 0.59 |
| 50| 10175 | 42.26 | 12.67 | 1.03 |
| 100| 14975 | 76.11 | 22.65 | 1.59 |
| 114| 16315 | 85.32 | 25.35 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 1788 | 18.67 | 6.48 | 0.43 |
| 2| 1919 | 19.79 | 7.48 | 0.45 |
| 3| 2051 | 20.85 | 8.46 | 0.47 |
| 5| 2316 | 23.08 | 10.46 | 0.51 |
| 10| 2966 | 28.67 | 15.46 | 0.62 |
| 50| 8209 | 75.76 | 56.01 | 1.53 |
| 71| 10961 | 99.24 | 76.97 | 1.99 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 590 | 16.97 | 6.03 | 0.36 |
| 2| 720 | 18.03 | 7.01 | 0.38 |
| 3| 852 | 19.12 | 8.01 | 0.40 |
| 5| 1119 | 21.35 | 10.00 | 0.44 |
| 10| 1770 | 26.95 | 15.01 | 0.55 |
| 50| 7011 | 72.47 | 55.14 | 1.44 |
| 72| 9895 | 98.43 | 77.46 | 1.94 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 597 | 21.80 | 7.20 | 0.40 |
| 2| 763 | 22.12 | 7.97 | 0.42 |
| 3| 864 | 24.99 | 9.46 | 0.46 |
| 5| 1130 | 28.32 | 11.75 | 0.51 |
| 10| 1819 | 36.12 | 17.30 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 625 | 25.14 | 8.14 | 0.44 |
| 2| 761 | 26.91 | 9.32 | 0.47 |
| 3| 891 | 28.68 | 10.50 | 0.50 |
| 5| 1158 | 32.47 | 12.91 | 0.56 |
| 10| 1813 | 42.86 | 19.19 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 1 | 57 | 5562 | 16.92 | 5.99 | 0.57 |
| 10 | 5 | 285 | 5698 | 24.14 | 9.00 | 0.66 |
| 10 | 20 | 1140 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1707 | 6547 | 69.61 | 27.91 | 1.20 |
| 10 | 40 | 2279 | 6890 | 87.85 | 35.49 | 1.42 |
| 10 | 46 | 2619 | 7091 | 98.80 | 40.03 | 1.55 |

