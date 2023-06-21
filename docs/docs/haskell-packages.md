---
sidebar_position: 99
---

# Haskell Packages

The Hydra project is divided into several Haskell packages fulfilling different parts of the protocol. While some packages are internal and specific to the Hydra project, some are quite generic and may be useful to other projects facing similar issues. Regardless, we expose [Haddock](https://www.haskell.org/haddock/) documentation for all of them.

## Public Packages

| Package                                                        | Description                                                                         |
| -------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| [plutus-merkle-tree](pathname://./haddock/plutus-merkle-tree/) | Implementation of Merkle Trees, compatible with on-chain Plutus validators.         |
| [plutus-cbor](pathname://./haddock/plutus-cbor/)               | Implementation of CBOR encoders, compatible with on-chain Plutus validators.        |
| [hydra-prelude](pathname://./haddock/hydra-prelude/)           | Custom Hydra Prelude used across other Hydra packages.                              |
| [hydra-cardano-api](pathname://./haddock/hydra-cardano-api/)   | A wrapper around the `cardano-api`, with era-specialized types and extra utilities. |

## Internal Packages

| Package                                                    | Description                                                             |
| ---------------------------------------------------------- | ----------------------------------------------------------------------- |
| [hydra-node](pathname://./haddock/hydra-node/)             | The Hydra node.                                                         |
| [hydra-node tests](pathname://./haddock/hydra-node/tests/) | The Hydra node test code.                                               |
| [hydra-tui](pathname://./haddock/hydra-tui/)               | Terminal User Interface (TUI) for managing a Hydra node                 |
| [hydra-plutus](pathname://./haddock/hydra-plutus/)         | Hydra Plutus Contracts                                                  |
| [hydra-cluster](pathname://./haddock/hydra-cluster/)       | Integration test suite using a local cluster of Cardano and hydra nodes |
