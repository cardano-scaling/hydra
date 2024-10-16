# Haskell packages

The Hydra project consists of several Haskell packages, each serving distinct parts of the protocol. While some packages are internal and tailored specifically to Hydra, others offer more generic functionalities that could benefit other projects tackling similar challenges. Comprehensive [Haddock](https://www.haskell.org/haddock/) documentation is provided for all packages.

## Public packages

| Package                                                                                        | Description                                                                         |
| ---                                                                                            | ---                                                                                 |
| [hydra-prelude](pathname:///haddock/hydra-prelude/index.html)           | Custom Hydra prelude used across other Hydra packages                              |
| [hydra-cardano-api](pathname:///haddock/hydra-cardano-api/index.html)   | A wrapper around the `cardano-api`, with era-specialized types and extra utilities |

## Internal packages

| Package                                                                                    | Description                                                             |
| ---                                                                                        | ---                                                                     |
| [hydra-node](pathname:///haddock/hydra-node/index.html)             | The Hydra node                                                         |
| [hydra-node tests](pathname:///haddock/hydra-node/tests/index.html) | The Hydra node test code                                               |
| [hydra-tui](pathname:///haddock/hydra-tui/index.html)               | Terminal User Interface (TUI) for managing a Hydra node                 |
| [hydra-plutus](pathname:///haddock/hydra-plutus/index.html)         | Hydra Plutus contracts                                                  |
| [hydra-cluster](pathname:///haddock/hydra-cluster/index.html)       | Integration test suite using a local cluster of Cardano and Hydra nodes |
