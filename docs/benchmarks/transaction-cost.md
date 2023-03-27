--- 
sidebar_label: 'Transactions Costs' 
sidebar_position: 3 
--- 

# Transactions Costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2023-03-27 14:12:00.150586 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νInitial | 9492414f8f96e8483a0b8ee268fc06a954641cb2cbaa9a8b093c2c9b | 4621 | 
| νCommit | 5d3f107aaa56d06188cf231941cf8163e777236a9cfdc48fd4bbfa23 | 2422 | 
| νHead | 82f16b51e2d81c6f4d42dd7398b4713a445464902f63dfd86ffe754e | 8954 | 
| μHead | 4083fa7081a0f4b4092fb02867c9ac594bb0e8bab8110ab242ba5a72* | 4458 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per Head.
## Cost of Init Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5052 | 15.67 | 6.22 | 0.54 |
| 2| 5259 | 15.47 | 6.05 | 0.55 |
| 3| 5465 | 20.18 | 7.91 | 0.61 |
| 5| 5875 | 23.03 | 8.92 | 0.66 |
| 10| 6897 | 35.51 | 13.65 | 0.84 |
| 37| 12434 | 99.23 | 37.70 | 1.77 |


## Cost of Commit Transaction
 Currently only one UTxO per commit allowed (this is about to change soon)

| UTxO | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :--- | ------: | --------: | --------: | --------: |
| 1| 603 | 15.83 | 6.22 | 0.35 |


## Cost of CollectCom Transaction

| Parties | UTxO (bytes) |Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :----------- |------: | --------: | --------: | --------: |
| 1 | 57 | 817 | 24.50 | 9.95 | 0.46 |
| 2 | 113 | 1138 | 40.41 | 16.53 | 0.65 |
| 3 | 171 | 1461 | 59.29 | 24.39 | 0.87 |
| 4 | 226 | 1791 | 80.79 | 33.37 | 1.12 |


## Cost of Close Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 640 | 17.34 | 7.44 | 0.37 |
| 2| 800 | 18.39 | 8.07 | 0.39 |
| 3| 965 | 19.82 | 8.86 | 0.42 |
| 5| 1307 | 22.91 | 10.52 | 0.47 |
| 10| 2133 | 29.88 | 14.37 | 0.59 |
| 50| 8725 | 86.40 | 45.46 | 1.56 |


## Cost of Contest Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 711 | 22.27 | 9.49 | 0.43 |
| 2| 841 | 23.49 | 10.01 | 0.45 |
| 3| 1006 | 25.59 | 11.04 | 0.48 |
| 5| 1337 | 29.11 | 12.84 | 0.54 |
| 10| 2161 | 37.25 | 17.09 | 0.67 |
| 45| 7929 | 97.60 | 48.15 | 1.63 |


## Cost of Abort Transaction
Some variation because of random mixture of still initial and already committed outputs.

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5028 | 26.22 | 10.98 | 0.66 |
| 2| 5492 | 47.99 | 20.73 | 0.93 |
| 3| 5816 | 70.08 | 30.47 | 1.19 |
| 4| 6138 | 95.71 | 41.76 | 1.49 |


## Cost of FanOut Transaction
Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 5 | 0 | 0 | 5083 | 10.59 | 4.43 | 0.49 |
| 5 | 1 | 57 | 5114 | 12.12 | 5.31 | 0.51 |
| 5 | 5 | 284 | 5257 | 18.22 | 8.83 | 0.59 |
| 5 | 10 | 570 | 5439 | 25.85 | 13.24 | 0.69 |
| 5 | 20 | 1139 | 5796 | 41.11 | 22.05 | 0.90 |
| 5 | 30 | 1704 | 6154 | 56.37 | 30.86 | 1.10 |
| 5 | 40 | 2273 | 6513 | 71.64 | 39.68 | 1.30 |
| 5 | 50 | 2847 | 6876 | 86.92 | 48.50 | 1.50 |
| 5 | 58 | 3305 | 7167 | 99.14 | 55.56 | 1.67 |

