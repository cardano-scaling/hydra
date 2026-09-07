# Haskell packages

The Hydra project consists of several Haskell packages, each serving distinct parts of the protocol. While some packages are internal and tailored specifically to Hydra, others offer more generic functionalities that could benefit other projects tackling similar challenges. [Haddock](https://www.haskell.org/haddock/) documentation is provided for most packages.

## Public packages

| Package                                                                                        | Description                                                                         |
| ---                                                                                            | ---                                                                                 |
| [hydra-prelude](pathname:///haddocks/hydra-prelude/index.html)           | Custom Hydra prelude used across other Hydra packages                              |
| [hydra-cardano-api](https://github.com/cardano-scaling/hydra/tree/master/hydra-cardano-api) | A wrapper around the `cardano-api`, with era-specialized types and extra utilities (haddocks unavailable: GHC 9.6.7 haddock panics on the re-exported `Cardano.Api` types) |

## Generic packages

Standalone libraries under `libs/`, not specific to Hydra:

| Package                                                                          | Description                                                            |
| ---                                                                              | ---                                                                    |
| [cborg-generic-tagged](pathname:///haddocks/cborg-generic-tagged/index.html)     | Constructor-name-tagged generic CBOR encoding                          |
| [contra-tracer-json](pathname:///haddocks/contra-tracer-json/index.html)         | Structured JSON logging over contra-tracer                             |
| [event-sourcing](pathname:///haddocks/event-sourcing/index.html)                 | Conduit-based event sourcing with a SQLite backend and log rotation    |
| [io-classes-labelled](pathname:///haddocks/io-classes-labelled/index.html)       | Label-attaching wrappers over io-classes STM and async primitives      |
| [persistent-queue](pathname:///haddocks/persistent-queue/index.html)             | A crash-safe, file-backed, bounded FIFO queue                          |
| [secret](pathname:///haddocks/secret/index.html)                                 | A type-level barrier for values that must never be shown or serialised |
| [test-network-ports](pathname:///haddocks/test-network-ports/index.html)         | Collision-free localhost TCP port allocation for test suites           |

## Internal packages

| Package                                                                                    | Description                                                             |
| ---                                                                                        | ---                                                                     |
| [hydra-node](pathname:///haddocks/hydra-node/index.html)             | The Hydra node                                                         |
| [hydra-node tests](pathname:///haddocks/hydra-node/tests/index.html) | The Hydra node test code                                               |
| [hydra-tx](pathname:///haddocks/hydra-tx/index.html)                 | Hydra transaction library                                               |
| [hydra-tui](pathname:///haddocks/hydra-tui/index.html)               | Terminal User Interface (TUI) for managing a Hydra node                 |
| [hydra-plutus](pathname:///haddocks/hydra-plutus/index.html)         | Hydra Plutus contracts                                                  |
| [hydra-cluster](pathname:///haddocks/hydra-cluster/index.html)       | Integration test suite using a local cluster of Cardano and Hydra nodes |
