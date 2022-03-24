---
sidebar_position: 99
---

# Haskell Packages

The Hydra project is divided into several Haskell packages fulfilling different parts of the protocol. While some packages are internal and specific to the Hydra project, some are quite generic and may be useful to other projects facing similar issues. Regardless, we expose [Haddock](https://www.haskell.org/haddock/) documentation for all of them. 

## Public Packages

| Package                                                                                        | Description                                                                         |
| ---                                                                                            | ---                                                                                 |
| [plutus-merkle-tree](https://hydra.family/head-protocol/haddock/plutus-merkle-tree/index.html) | Implementation of Merkle Trees, compatible with on-chain Plutus validators.         |
| [plutus-cbor](https://hydra.family/head-protocol/haddock/plutus-cbor/index.html)               | Implementation of CBOR encoders, compatible with on-chain Plutus validators.        |
| [hydra-prelude](https://hydra.family/head-protocol/haddock/hydra-prelude/index.html)           | Custom Hydra Prelude used across other Hydra packages.                              |
| [hydra-cardano-api](https://hydra.family/head-protocol/haddock/hydra-cardano-api/index.html)   | A wrapper around the `cardano-api`, with era-specialized types and extra utilities. |

## Internal Packages

| Package                                                                                    | Description                                                             |
| ---                                                                                        | ---                                                                     |
| [hydra-node](https://hydra.family/head-protocol/haddock/hydra-node/index.html)             | The Hydra node.                                                         |
| [hydra-node tests](https://hydra.family/head-protocol/haddock/hydra-node/tests/index.html) | The Hydra node test code.                                               |
| [hydra-tui](https://hydra.family/head-protocol/haddock/hydra-tui/index.html)               | Terminal User Interface (TUI) for managing a Hydra node                 |
| [hydra-plutus](https://hydra.family/head-protocol/haddock/hydra-plutus/index.html)         | Hydra Plutus Contracts                                                  |
| [hydra-cluster](https://hydra.family/head-protocol/haddock/hydra-cluster/index.html)       | Integration test suite using a local cluster of Cardano and hydra nodes |
