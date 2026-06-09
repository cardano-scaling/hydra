--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-09 13:29:43.371382568 UTC |
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
| 2| 5500 | 9.18 | 3.01 | 0.49 |
| 3| 5594 | 10.08 | 3.31 | 0.50 |
| 5| 5788 | 11.03 | 3.61 | 0.52 |
| 10| 6268 | 13.13 | 4.27 | 0.56 |
| 50| 10108 | 34.61 | 10.99 | 0.95 |
| 100| 14911 | 62.23 | 19.66 | 1.45 |
| 115| 16348 | 69.72 | 21.98 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2304 | 12.40 | 4.85 | 0.39 |
| 2| 2434 | 13.23 | 5.84 | 0.41 |
| 3| 2567 | 13.76 | 6.71 | 0.42 |
| 5| 2829 | 14.75 | 8.40 | 0.45 |
| 10| 3484 | 18.13 | 13.01 | 0.54 |
| 50| 8724 | 43.21 | 48.82 | 1.24 |
| 75| 11999 | 58.25 | 71.05 | 1.66 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 624 | 10.30 | 4.21 | 0.29 |
| 2| 755 | 10.87 | 5.09 | 0.31 |
| 3| 886 | 11.43 | 5.96 | 0.32 |
| 5| 1149 | 12.58 | 7.73 | 0.36 |
| 10| 1804 | 15.31 | 12.10 | 0.44 |
| 50| 7045 | 39.55 | 47.67 | 1.12 |
| 75| 10321 | 54.55 | 69.87 | 1.55 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 685 | 18.93 | 12.53 | 0.42 |
| 2| 817 | 20.12 | 13.59 | 0.45 |
| 3| 944 | 21.37 | 14.67 | 0.47 |
| 5| 1176 | 25.23 | 17.76 | 0.53 |
| 10| 1831 | 33.12 | 23.60 | 0.67 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 709 | 22.65 | 15.77 | 0.48 |
| 2| 845 | 24.04 | 16.89 | 0.50 |
| 3| 975 | 25.44 | 18.01 | 0.53 |
| 5| 1237 | 28.43 | 20.31 | 0.58 |
| 10| 1892 | 36.85 | 26.31 | 0.72 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5433 | 14.40 | 5.63 | 0.55 |
| 10 | 1 | 57 | 5467 | 16.20 | 6.39 | 0.57 |
| 10 | 5 | 286 | 5604 | 23.41 | 9.44 | 0.65 |
| 10 | 30 | 1709 | 6455 | 68.82 | 28.60 | 1.20 |
| 10 | 40 | 2277 | 6792 | 87.03 | 36.27 | 1.41 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 974 | 32.72 | 63.62 | 0.92 |
| 25 | 1367 | 1456 | 69.03 | 99.36 | 1.49 |
| 30 | 1363 | 1453 | 69.03 | 99.36 | 1.49 |
| 40 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |
| 50 | 1364 | 1454 | 69.03 | 99.36 | 1.49 |
| 100 | 1365 | 1455 | 69.03 | 99.36 | 1.49 |
| 150 | 1367 | 1457 | 69.03 | 99.36 | 1.49 |
| 200 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |
| 200 | 1366 | 1456 | 69.03 | 99.36 | 1.49 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1070 | 1545 | 39.79 | 66.14 | 1.02 |
| 25 | 2134 | 2351 | 77.90 | 99.28 | 1.60 |
| 30 | 2068 | 2282 | 77.88 | 99.26 | 1.60 |
| 40 | 2134 | 2351 | 77.90 | 99.28 | 1.60 |
| 50 | 2134 | 2352 | 77.90 | 99.28 | 1.60 |
| 100 | 2134 | 2352 | 77.90 | 99.28 | 1.60 |
| 150 | 2486 | 2720 | 77.90 | 99.38 | 1.62 |
| 200 | 2706 | 2950 | 77.88 | 99.43 | 1.63 |
| 200 | 2178 | 2398 | 77.90 | 99.28 | 1.60 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 107 | 5438 | 22.05 | 43.33 | 0.88 |
| 5 | 485 | 5733 | 35.66 | 54.85 | 1.09 |
| 10 | 940 | 6082 | 53.86 | 69.63 | 1.35 |
| 10 | 1080 | 6222 | 53.86 | 69.65 | 1.36 |

