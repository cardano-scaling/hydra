--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-07-15 12:31:05.06991917 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 2b91a7e666575a2465b8c7f6a7f960d5870cf13694a67f3215e014c5 | 12511 | 
| μHead | f2620ca915623f152a9d966b32364c416d31e1b3874065c3eaee999b* | 4856 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5352 | 8.85 | 2.91 | 0.48 |
| 2| 5447 | 9.87 | 3.26 | 0.49 |
| 3| 5544 | 9.63 | 3.15 | 0.49 |
| 5| 5736 | 10.63 | 3.47 | 0.51 |
| 10| 6217 | 13.35 | 4.34 | 0.56 |
| 50| 10059 | 34.61 | 10.99 | 0.95 |
| 100| 14859 | 61.73 | 19.50 | 1.44 |
| 115| 16298 | 69.63 | 21.96 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2312 | 19.18 | 6.90 | 0.46 |
| 2| 2448 | 19.59 | 7.69 | 0.47 |
| 3| 2574 | 20.51 | 8.65 | 0.49 |
| 5| 2837 | 22.84 | 10.73 | 0.54 |
| 10| 3490 | 27.54 | 15.57 | 0.64 |
| 50| 8734 | 67.14 | 54.64 | 1.47 |
| 75| 12007 | 92.30 | 79.18 | 2.00 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 634 | 16.44 | 6.04 | 0.35 |
| 2| 766 | 17.36 | 7.00 | 0.37 |
| 3| 895 | 18.26 | 7.96 | 0.39 |
| 5| 1157 | 20.09 | 9.87 | 0.43 |
| 10| 1813 | 24.65 | 14.66 | 0.53 |
| 50| 7055 | 62.92 | 53.33 | 1.35 |
| 75| 10328 | 87.86 | 77.78 | 1.88 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 2| 792 | 16.55 | 11.42 | 0.40 |
| 3| 922 | 17.48 | 12.39 | 0.42 |
| 5| 1185 | 19.31 | 14.31 | 0.46 |
| 10| 1840 | 23.85 | 19.10 | 0.56 |
| 50| 7082 | 62.99 | 58.11 | 1.39 |
| 74| 10225 | 86.80 | 81.60 | 1.89 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 687 | 18.93 | 13.57 | 0.43 |
| 2| 822 | 20.00 | 14.58 | 0.45 |
| 3| 954 | 21.10 | 15.59 | 0.48 |
| 5| 1217 | 23.33 | 17.63 | 0.52 |
| 10| 1872 | 28.76 | 22.69 | 0.63 |
| 50| 7112 | 74.84 | 63.75 | 1.53 |
| 71| 9864 | 98.79 | 85.24 | 2.00 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5530 | 23.24 | 42.86 | 0.89 |
| 10 | 1 | 57 | 5563 | 25.58 | 45.37 | 0.93 |
| 10 | 5 | 285 | 5699 | 35.83 | 55.69 | 1.09 |
| 10 | 10 | 569 | 5868 | 49.84 | 68.98 | 1.31 |
| 10 | 20 | 1140 | 6210 | 82.37 | 96.96 | 1.79 |
| 10 | 20 | 1140 | 6209 | 82.37 | 96.96 | 1.79 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.86 | 66.32 | 0.95 |
| 25 | 1309 | 1428 | 68.19 | 99.38 | 1.48 |
| 30 | 1309 | 1428 | 68.19 | 99.38 | 1.48 |
| 40 | 1310 | 1429 | 68.19 | 99.38 | 1.48 |
| 50 | 1309 | 1428 | 68.19 | 99.38 | 1.48 |
| 100 | 1310 | 1429 | 68.19 | 99.38 | 1.48 |
| 150 | 1306 | 1425 | 68.19 | 99.38 | 1.48 |
| 200 | 1309 | 1428 | 68.19 | 99.38 | 1.48 |
| 200 | 1308 | 1427 | 68.19 | 99.38 | 1.48 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 990 | 1466 | 41.93 | 68.81 | 1.05 |
| 25 | 2016 | 2258 | 76.54 | 99.10 | 1.58 |
| 30 | 2478 | 2742 | 76.54 | 99.21 | 1.60 |
| 40 | 2268 | 2522 | 76.52 | 99.15 | 1.59 |
| 50 | 2520 | 2787 | 76.52 | 99.24 | 1.61 |
| 100 | 1974 | 2215 | 76.52 | 99.09 | 1.58 |
| 150 | 2289 | 2545 | 76.54 | 99.16 | 1.60 |
| 200 | 2121 | 2369 | 76.54 | 99.11 | 1.59 |
| 200 | 2079 | 2325 | 76.54 | 99.11 | 1.59 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 99 | 5403 | 21.97 | 44.27 | 0.88 |
| 5 | 480 | 5700 | 35.70 | 55.82 | 1.09 |
| 10 | 1180 | 6296 | 53.90 | 70.65 | 1.37 |
| 10 | 1130 | 6246 | 53.78 | 70.62 | 1.37 |

