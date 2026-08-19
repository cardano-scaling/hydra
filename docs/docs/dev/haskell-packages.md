# Haskell packages

The Hydra project consists of several Haskell packages, each serving distinct parts of the protocol. While some packages are internal and tailored specifically to Hydra, others offer more generic functionalities that could benefit other projects tackling similar challenges. [Haddock](https://www.haskell.org/haddock/) documentation is provided for most packages.

## Public packages

| Package                                                                                        | Description                                                                         |
| ---                                                                                            | ---                                                                                 |
| [hydra-prelude](pathname:///haddocks/hydra-prelude/index.html)           | Custom Hydra prelude used across other Hydra packages                              |
| [hydra-cardano-api](https://github.com/cardano-scaling/hydra/tree/master/hydra-cardano-api) | A wrapper around the `cardano-api`, with era-specialized types and extra utilities (haddocks unavailable: GHC 9.6.7 haddock panics on the re-exported `Cardano.Api` types) |

## Internal packages

| Package                                                                                    | Description                                                             |
| ---                                                                                        | ---                                                                     |
| [hydra-node](pathname:///haddocks/hydra-node/index.html)             | The Hydra node                                                         |
| [hydra-node tests](pathname:///haddocks/hydra-node/tests/index.html) | The Hydra node test code                                               |
| [hydra-tx](pathname:///haddocks/hydra-tx/index.html)                 | Hydra transaction library                                               |
| [hydra-tui](pathname:///haddocks/hydra-tui/index.html)               | Terminal User Interface (TUI) for managing a Hydra node                 |
| [hydra-plutus](pathname:///haddocks/hydra-plutus/index.html)         | Hydra Plutus contracts                                                  |
| [hydra-cluster](pathname:///haddocks/hydra-cluster/index.html)       | Integration test suite using a local cluster of Cardano and Hydra nodes |
