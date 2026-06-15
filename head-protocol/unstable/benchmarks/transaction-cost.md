--- 
sidebar_label: 'Transaction costs' 
sidebar_position: 3 
--- 

# Transaction costs 

Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using `arbitrary` values and results are not fully deterministic and comparable to previous runs.

| Metadata | |
| :--- | :--- |
| _Generated at_ | 2026-06-15 15:15:53.889150209 UTC |
| _Max. memory units_ | 14000000 |
| _Max. CPU units_ | 10000000000 |
| _Max. tx size (kB)_ | 16384 |

## Script summary

| Name   | Hash | Size (Bytes) 
| :----- | :--- | -----------: 
| νHead | fd75e24c9ea915ce8e48d3ff1d0c54ad09cc01191c24416ad7dba4a3 | 11621 | 
| μHead | 83a964e973c065bbe70588f5e089817f92182ae81743e7a54cf3e29e* | 4856 | 
| νDeposit | c78e8c9205721eb3ef4410f3db9c6169fa6db497c24641d29c20529c | 1615 | 
| νCRS | 09db7ee6cf7a4b358dd5c8a2f19d2c048336ffc5a01ef35a47ca7072 | 2736 | 

* The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per head.

## `Init` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 5352 | 8.73 | 2.87 | 0.48 |
| 2| 5445 | 9.16 | 3.00 | 0.49 |
| 3| 5541 | 9.65 | 3.15 | 0.49 |
| 5| 5736 | 10.69 | 3.49 | 0.51 |
| 10| 6220 | 13.70 | 4.47 | 0.57 |
| 50| 10058 | 35.06 | 11.14 | 0.96 |
| 100| 14858 | 61.62 | 19.44 | 1.44 |
| 115| 16299 | 69.55 | 21.93 | 1.59 |


## Cost of Increment Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 2278 | 18.41 | 6.62 | 0.45 |
| 2| 2409 | 18.65 | 7.30 | 0.46 |
| 3| 2540 | 19.62 | 8.22 | 0.48 |
| 5| 2803 | 21.51 | 10.06 | 0.52 |
| 10| 3458 | 25.98 | 14.54 | 0.62 |
| 50| 8697 | 64.90 | 51.30 | 1.43 |
| 75| 11974 | 89.87 | 74.46 | 1.94 |


## Cost of Decrement Transaction

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 598 | 16.03 | 5.75 | 0.35 |
| 2| 730 | 16.94 | 6.65 | 0.37 |
| 3| 861 | 17.83 | 7.55 | 0.39 |
| 5| 1124 | 19.62 | 9.35 | 0.43 |
| 10| 1778 | 24.03 | 13.82 | 0.52 |
| 50| 7020 | 62.46 | 50.39 | 1.33 |
| 75| 10295 | 86.57 | 73.26 | 1.83 |


## `Close` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 592 | 15.49 | 10.37 | 0.38 |
| 2| 724 | 16.37 | 11.27 | 0.40 |
| 5| 1117 | 19.14 | 14.00 | 0.46 |
| 10| 1776 | 23.70 | 18.52 | 0.55 |
| 50| 7014 | 62.16 | 55.19 | 1.36 |
| 75| 10289 | 86.60 | 78.22 | 1.87 |


## `Contest` transaction costs

| Parties | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | ------: | --------: | --------: | --------: |
| 1| 623 | 18.79 | 13.48 | 0.43 |
| 2| 750 | 19.85 | 14.43 | 0.45 |
| 3| 885 | 20.92 | 15.38 | 0.47 |
| 5| 1144 | 23.05 | 17.29 | 0.51 |
| 10| 1808 | 28.43 | 22.06 | 0.62 |
| 50| 7041 | 73.64 | 60.74 | 1.49 |
| 73| 10058 | 99.45 | 82.92 | 1.99 |


## `FanOut` transaction costs
Involves spending head output and burning head tokens. Uses ada-only UTXO for better comparability.

| Parties | UTxO  | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| :------ | :---- | :----------- | ------: | --------: | --------: | --------: |
| 10 | 0 | 0 | 5529 | 22.68 | 41.73 | 0.88 |
| 10 | 1 | 57 | 5563 | 25.03 | 44.25 | 0.92 |
| 10 | 5 | 284 | 5698 | 35.28 | 54.57 | 1.08 |
| 10 | 10 | 569 | 5868 | 49.29 | 67.85 | 1.30 |
| 10 | 20 | 1137 | 6206 | 81.82 | 95.83 | 1.78 |
| 10 | 20 | 1140 | 6209 | 81.82 | 95.83 | 1.78 |


## `PartialFanOut` transaction costs
Largest chunk of ada-only outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 569 | 982 | 34.31 | 65.19 | 0.94 |
| 25 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 30 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 40 | 1308 | 1427 | 67.64 | 98.26 | 1.47 |
| 50 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |
| 100 | 1311 | 1426 | 67.64 | 98.26 | 1.47 |
| 150 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1310 | 1429 | 67.64 | 98.26 | 1.47 |
| 200 | 1309 | 1428 | 67.64 | 98.26 | 1.47 |


## `PartialFanOut` transaction costs (with native tokens)
Largest chunk of native-token outputs that can be distributed in one partial fanout step, computed dynamically. The last row is the maximum total UTxO count where at least one output can still be distributed.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 11 | 1060 | 1539 | 41.40 | 67.72 | 1.04 |
| 25 | 2520 | 2786 | 75.99 | 98.13 | 1.59 |
| 30 | 2415 | 2676 | 75.99 | 98.08 | 1.59 |
| 40 | 1974 | 2214 | 75.97 | 97.97 | 1.57 |
| 50 | 1995 | 2237 | 75.97 | 97.97 | 1.57 |
| 100 | 2079 | 2321 | 75.99 | 97.98 | 1.57 |
| 150 | 2079 | 2325 | 75.99 | 97.98 | 1.57 |
| 200 | 2478 | 2743 | 75.97 | 98.08 | 1.59 |
| 200 | 2058 | 2303 | 75.97 | 97.98 | 1.57 |


## `FinalPartialFanOut` transaction costs (with native tokens)
Terminal partial fanout step (FanoutProgress → Final) with outputs carrying a native token. Burns all head tokens and proves accumulator exhaustion via BLS proof.

| Distributed | UTxO (bytes) | Tx size | % max Mem | % max CPU | Min fee ₳ |
| ----------: | -----------: | ------: | --------: | --------: | --------: |
| 1 | 106 | 5410 | 21.54 | 43.18 | 0.87 |
| 5 | 600 | 5820 | 35.15 | 54.73 | 1.08 |
| 10 | 1150 | 6265 | 53.35 | 69.53 | 1.36 |
| 10 | 1050 | 6165 | 53.35 | 69.50 | 1.35 |

