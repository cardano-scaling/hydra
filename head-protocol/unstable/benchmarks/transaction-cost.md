--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-05-11 10:07:40.5924338 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 3116bfd2081d13d82cb39aa366a9ec2ee1cc0813dcd22cd406af799d | 11551 | 
| μHead | 30b675f68271d2882a20d34fa3729fc8ed505fcd0f70a655c56d24e7* | 4978 | 
| νDeposit | 0430c1afbb704085b936bd7fd530b5c674009c0b85fd18e7a8494ad8 | 1615 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5464 | 9.98 | 3.15 | 0.49 |
| 2| 5562 | 10.39 | 3.26 | 0.50 |
| 3| 5656 | 10.85 | 3.39 | 0.51 |
| 5| 5847 | 12.12 | 3.77 | 0.53 |
| 10| 6327 | 15.28 | 4.71 | 0.59 |
| 50| 10171 | 42.44 | 12.72 | 1.03 |
| 100| 14972 | 76.02 | 22.60 | 1.59 |
| 114| 16313 | 85.79 | 25.51 | 1.74 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2271 | 19.37 | 6.74 | 0.46 |
| 2| 2401 | 21.08 | 7.93 | 0.48 |
| 3| 2532 | 21.75 | 8.78 | 0.50 |
| 5| 2795 | 24.45 | 10.93 | 0.55 |
| 10| 3451 | 29.79 | 15.85 | 0.66 |
| 50| 8690 | 77.10 | 56.52 | 1.56 |
| 68| 11047 | 97.72 | 74.65 | 1.96 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 595 | 17.00 | 6.03 | 0.36 |
| 2| 721 | 18.11 | 7.03 | 0.38 |
| 3| 852 | 19.23 | 8.03 | 0.40 |
| 5| 1115 | 21.43 | 10.02 | 0.44 |
| 10| 1770 | 26.77 | 14.96 | 0.55 |
| 50| 7011 | 73.22 | 55.34 | 1.45 |
| 73| 10024 | 99.15 | 78.35 | 1.96 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 632 | 20.68 | 6.88 | 0.39 |
| 2| 737 | 23.40 | 8.33 | 0.43 |
| 3| 865 | 25.01 | 9.46 | 0.46 |
| 5| 1157 | 26.98 | 11.37 | 0.50 |
| 10| 1781 | 37.94 | 17.81 | 0.66 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 629 | 25.20 | 8.16 | 0.44 |
| 2| 765 | 26.92 | 9.32 | 0.47 |
| 3| 891 | 28.69 | 10.50 | 0.50 |
| 5| 1154 | 32.48 | 12.91 | 0.56 |
| 10| 1809 | 42.71 | 19.15 | 0.71 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5528 | 15.12 | 5.24 | 0.55 |
| 10 | 1 | 57 | 5563 | 16.92 | 5.99 | 0.57 |
| 10 | 10 | 569 | 5867 | 33.17 | 12.77 | 0.77 |
| 10 | 20 | 1140 | 6208 | 51.38 | 20.34 | 0.99 |
| 10 | 30 | 1706 | 6547 | 69.62 | 27.91 | 1.20 |
| 10 | 46 | 2616 | 7089 | 98.80 | 40.04 | 1.55 |

