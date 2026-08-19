--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-08-03 09:45:53.177161038 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | f2dd4ade71e19c2310a86215aa78aea06463aca2d8b818af8dc1b8a4 | 12805 | 
| μHead | 4abb8dedbcd6a6f03f4fe227300e2713d73b7680d47baa898b60d27a* | 4971 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5476 | 9.71 | 3.20 | 0.49 |
| 2| 5572 | 10.12 | 3.32 | 0.50 |
| 3| 5667 | 10.29 | 3.37 | 0.51 |
| 5| 5861 | 11.15 | 3.63 | 0.52 |
| 10| 6340 | 13.66 | 4.44 | 0.57 |
| 50| 10182 | 35.72 | 11.37 | 0.97 |
| 100| 14980 | 62.04 | 19.57 | 1.45 |
| 114| 16327 | 69.72 | 21.99 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2318 | 21.15 | 7.52 | 0.48 |
| 2| 2453 | 22.23 | 8.53 | 0.50 |
| 3| 2582 | 22.54 | 9.28 | 0.51 |
| 5| 2846 | 25.11 | 11.44 | 0.56 |
| 10| 3500 | 29.45 | 16.15 | 0.66 |
| 50| 8742 | 71.88 | 56.09 | 1.52 |
| 75| 12018 | 97.11 | 80.63 | 2.04 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 642 | 18.46 | 6.68 | 0.38 |
| 2| 778 | 19.40 | 7.64 | 0.40 |
| 3| 904 | 20.39 | 8.62 | 0.42 |
| 5| 1171 | 22.35 | 10.58 | 0.46 |
| 10| 1826 | 27.08 | 15.42 | 0.56 |
| 50| 7062 | 67.92 | 54.84 | 1.41 |
| 75| 10338 | 93.55 | 79.51 | 1.93 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 669 | 17.76 | 11.66 | 0.41 |
| 3| 931 | 19.72 | 13.62 | 0.45 |
| 5| 1194 | 21.62 | 15.56 | 0.49 |
| 10| 1845 | 26.60 | 20.48 | 0.59 |
| 50| 7090 | 67.23 | 59.94 | 1.44 |
| 75| 10365 | 93.24 | 84.77 | 1.97 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 700 | 21.48 | 14.90 | 0.46 |
| 2| 831 | 22.66 | 15.94 | 0.48 |
| 3| 963 | 23.77 | 16.96 | 0.51 |
| 5| 1221 | 26.07 | 19.02 | 0.55 |
| 10| 1881 | 31.79 | 24.16 | 0.66 |
| 50| 7121 | 79.53 | 65.73 | 1.58 |
| 66| 9214 | 98.42 | 82.30 | 1.94 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.
Rows first grow the UTxO set at a fixed 10 parties, then show the largest set that still fits per number of parties (burning more participation tokens leaves less room for outputs).

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5644 | 23.29 | 42.87 | 0.90 |
| 10 | 1 | 56 | 5677 | 25.63 | 45.39 | 0.93 |
| 10 | 5 | 284 | 5813 | 35.88 | 55.71 | 1.10 |
| 10 | 10 | 569 | 5983 | 49.89 | 68.99 | 1.31 |
| 10 | 20 | 1137 | 6321 | 82.42 | 96.97 | 1.79 |
| 1 | 20 | 1139 | 6045 | 76.20 | 95.00 | 1.72 |
| 5 | 20 | 1139 | 6168 | 78.96 | 95.88 | 1.75 |
| 10 | 20 | 1138 | 6323 | 82.42 | 96.97 | 1.79 |
| 20 | 20 | 1140 | 6634 | 89.74 | 99.26 | 1.88 |
| 50 | 15 | 855 | 7395 | 94.70 | 91.89 | 1.90 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 570 | 987 | 34.91 | 66.33 | 0.95 |
| 25 | 1308 | 1427 | 68.24 | 99.40 | 1.48 |
| 30 | 1309 | 1428 | 68.24 | 99.40 | 1.48 |
| 40 | 1309 | 1428 | 68.24 | 99.40 | 1.48 |
| 50 | 1306 | 1425 | 68.24 | 99.40 | 1.48 |
| 100 | 1310 | 1429 | 68.24 | 99.40 | 1.48 |
| 150 | 1308 | 1427 | 68.24 | 99.40 | 1.48 |
| 200 | 1307 | 1426 | 68.24 | 99.40 | 1.48 |
| 200 | 1311 | 1426 | 68.24 | 99.40 | 1.48 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1180 | 1675 | 42.00 | 68.88 | 1.06 |
| 25 | 2478 | 2742 | 76.59 | 99.22 | 1.61 |
| 30 | 2226 | 2478 | 76.59 | 99.17 | 1.59 |
| 40 | 1953 | 2192 | 76.59 | 99.12 | 1.58 |
| 50 | 2226 | 2479 | 76.59 | 99.17 | 1.59 |
| 100 | 2037 | 2281 | 76.59 | 99.12 | 1.58 |
| 150 | 2436 | 2699 | 76.56 | 99.22 | 1.60 |
| 200 | 1953 | 2193 | 76.56 | 99.11 | 1.58 |
| 200 | 1974 | 2211 | 76.59 | 99.12 | 1.58 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 118 | 5537 | 22.10 | 44.31 | 0.89 |
| 5 | 590 | 5925 | 35.70 | 55.84 | 1.10 |
| 10 | 1060 | 6291 | 53.90 | 70.63 | 1.37 |
| 10 | 1160 | 6391 | 53.78 | 70.62 | 1.38 |

