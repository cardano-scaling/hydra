--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-11 08:58:36.25222528 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | b50bdb59e7d93e1a941222d8d638709a0387117316979fd60fec9c11 | 13311 | 
| μHead | 1501b8219bd47bfc33886c92832c9f21bb48701c5d384a539abbb491* | 4883 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5403 | 8.75 | 2.87 | 0.48 |
| 2| 5500 | 9.55 | 3.14 | 0.49 |
| 3| 5596 | 9.69 | 3.17 | 0.50 |
| 5| 5789 | 10.67 | 3.48 | 0.52 |
| 10| 6266 | 13.33 | 4.34 | 0.57 |
| 50| 10110 | 34.90 | 11.10 | 0.96 |
| 100| 14911 | 61.80 | 19.50 | 1.45 |
| 115| 16351 | 69.80 | 22.01 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2303 | 19.13 | 6.92 | 0.46 |
| 2| 2434 | 19.98 | 7.88 | 0.48 |
| 3| 2566 | 20.91 | 8.87 | 0.50 |
| 5| 2829 | 22.33 | 10.70 | 0.53 |
| 10| 3488 | 27.36 | 15.77 | 0.64 |
| 50| 8725 | 67.30 | 56.05 | 1.48 |
| 75| 12000 | 92.07 | 81.14 | 2.01 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 628 | 16.39 | 6.06 | 0.35 |
| 2| 755 | 17.28 | 7.04 | 0.37 |
| 3| 887 | 18.24 | 8.03 | 0.39 |
| 5| 1149 | 20.03 | 9.99 | 0.43 |
| 10| 1803 | 24.72 | 14.95 | 0.54 |
| 50| 7044 | 63.98 | 54.97 | 1.37 |
| 75| 10320 | 88.07 | 79.85 | 1.89 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 647 | 19.87 | 13.34 | 0.44 |
| 2| 817 | 20.17 | 13.60 | 0.45 |
| 3| 949 | 21.41 | 14.68 | 0.47 |
| 5| 1211 | 24.12 | 16.89 | 0.52 |
| 10| 1861 | 31.68 | 22.64 | 0.65 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 713 | 22.64 | 15.76 | 0.48 |
| 2| 845 | 24.05 | 16.89 | 0.50 |
| 3| 975 | 25.46 | 18.02 | 0.53 |
| 5| 1238 | 28.45 | 20.32 | 0.58 |
| 10| 1893 | 36.96 | 26.34 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5434 | 14.40 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.20 | 6.39 | 0.57 |
| 10 | 5 | 285 | 5604 | 23.41 | 9.44 | 0.65 |
| 10 | 10 | 568 | 5771 | 32.43 | 13.26 | 0.76 |
| 10 | 20 | 1139 | 6113 | 50.61 | 20.92 | 0.98 |
| 10 | 30 | 1706 | 6452 | 68.82 | 28.60 | 1.20 |
| 10 | 30 | 1706 | 6452 | 68.82 | 28.60 | 1.20 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 978 | 32.72 | 63.62 | 0.92 |
| 25 | 1367 | 1456 | 69.03 | 99.36 | 1.49 |
| 30 | 1368 | 1454 | 69.03 | 99.36 | 1.49 |
| 40 | 1365 | 1455 | 69.03 | 99.36 | 1.49 |
| 50 | 1368 | 1458 | 69.03 | 99.36 | 1.49 |
| 100 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |
| 150 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |
| 200 | 1365 | 1455 | 69.03 | 99.36 | 1.49 |
| 200 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1090 | 1571 | 39.81 | 66.14 | 1.02 |
| 25 | 2046 | 2259 | 77.90 | 99.27 | 1.59 |
| 30 | 2046 | 2255 | 77.90 | 99.27 | 1.59 |
| 40 | 2134 | 2351 | 77.90 | 99.28 | 1.60 |
| 50 | 2596 | 2835 | 77.90 | 99.38 | 1.62 |
| 100 | 2134 | 2352 | 77.88 | 99.27 | 1.60 |
| 150 | 2090 | 2306 | 77.90 | 99.27 | 1.60 |
| 200 | 2662 | 2904 | 77.91 | 99.43 | 1.62 |
| 200 | 2112 | 2329 | 77.90 | 99.27 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 109 | 5441 | 22.05 | 43.33 | 0.88 |
| 5 | 465 | 5712 | 35.66 | 54.85 | 1.09 |
| 10 | 930 | 6072 | 53.86 | 69.63 | 1.35 |
| 10 | 1150 | 6292 | 53.86 | 69.68 | 1.36 |

