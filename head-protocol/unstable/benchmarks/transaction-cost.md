--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-08 09:19:14.774612433 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | 0516e88607e9f014c5d78370663e730b1f84528b4beaae01ef34bdaf | 13263 | 
| μHead | a46f080d4c2c01a115a50e5542f92db11eb6f082e088b9a8ac956f0d* | 4883 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5403 | 9.04 | 2.98 | 0.48 |
| 2| 5503 | 10.01 | 3.31 | 0.50 |
| 3| 5594 | 10.06 | 3.30 | 0.50 |
| 5| 5786 | 11.09 | 3.63 | 0.52 |
| 10| 6272 | 13.43 | 4.37 | 0.57 |
| 50| 10111 | 34.55 | 10.97 | 0.95 |
| 100| 14911 | 61.71 | 19.48 | 1.45 |
| 115| 16348 | 69.47 | 21.90 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2304 | 12.66 | 4.96 | 0.39 |
| 2| 2434 | 13.33 | 5.87 | 0.41 |
| 3| 2570 | 13.62 | 6.64 | 0.42 |
| 5| 2827 | 14.88 | 8.44 | 0.46 |
| 10| 3486 | 17.91 | 12.91 | 0.54 |
| 50| 8726 | 42.47 | 48.62 | 1.23 |
| 75| 11999 | 58.76 | 71.21 | 1.67 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 10.30 | 4.21 | 0.29 |
| 2| 755 | 10.86 | 5.08 | 0.31 |
| 3| 886 | 11.41 | 5.96 | 0.32 |
| 5| 1149 | 12.51 | 7.71 | 0.36 |
| 10| 1807 | 15.41 | 12.13 | 0.44 |
| 50| 7050 | 39.17 | 47.57 | 1.12 |
| 75| 10320 | 54.21 | 69.76 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 19.87 | 13.34 | 0.44 |
| 2| 788 | 21.15 | 14.43 | 0.46 |
| 3| 948 | 21.43 | 14.68 | 0.47 |
| 5| 1176 | 25.27 | 17.77 | 0.53 |
| 10| 1835 | 33.12 | 23.60 | 0.67 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 22.65 | 15.77 | 0.48 |
| 2| 848 | 24.05 | 16.89 | 0.50 |
| 3| 971 | 25.49 | 18.03 | 0.53 |
| 5| 1238 | 28.50 | 20.33 | 0.58 |
| 10| 1897 | 36.94 | 26.34 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5434 | 14.40 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.20 | 6.39 | 0.57 |
| 10 | 5 | 285 | 5603 | 23.41 | 9.44 | 0.65 |
| 10 | 10 | 570 | 5773 | 32.43 | 13.26 | 0.76 |
| 10 | 20 | 1137 | 6110 | 50.61 | 20.92 | 0.98 |
| 10 | 30 | 1706 | 6451 | 68.82 | 28.60 | 1.20 |
| 10 | 40 | 2276 | 6791 | 87.03 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 568 | 977 | 32.72 | 63.62 | 0.92 |
| 25 | 1368 | 1456 | 69.03 | 99.36 | 1.49 |
| 30 | 1365 | 1451 | 69.03 | 99.36 | 1.49 |
| 40 | 1368 | 1454 | 69.03 | 99.36 | 1.49 |
| 50 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |
| 100 | 1361 | 1451 | 69.03 | 99.36 | 1.49 |
| 150 | 1363 | 1453 | 69.03 | 99.36 | 1.49 |
| 200 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |
| 200 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1160 | 1648 | 39.79 | 66.16 | 1.02 |
| 25 | 2178 | 2397 | 77.90 | 99.28 | 1.60 |
| 30 | 2530 | 2761 | 77.90 | 99.38 | 1.62 |
| 40 | 2156 | 2370 | 77.90 | 99.28 | 1.60 |
| 50 | 2178 | 2398 | 77.90 | 99.28 | 1.60 |
| 100 | 2046 | 2260 | 77.90 | 99.27 | 1.59 |
| 150 | 2552 | 2789 | 77.90 | 99.38 | 1.62 |
| 200 | 2354 | 2582 | 77.90 | 99.33 | 1.61 |
| 200 | 2090 | 2306 | 77.90 | 99.27 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 116 | 5447 | 22.05 | 43.33 | 0.88 |
| 5 | 495 | 5742 | 35.66 | 54.85 | 1.09 |
| 10 | 1150 | 6292 | 53.86 | 69.68 | 1.36 |
| 10 | 1190 | 6333 | 53.86 | 69.70 | 1.37 |

