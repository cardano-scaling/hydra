# Architecture Decision Records

Here is a list of currently accepted or discussed **Architecture Decision Records**, which serve as a lightweight technical documentation for the Hydra node software. As new ADRs are added and old ones are deprecated or superseded, this list should be updated to reflect the current situation. Follow the links to have a detailed explanation for each ADR.

1. Principles and patterns are documented using [Architecture Decision Records](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0001-record-architecture-decisions.md)
2. Hydra Node works is designed around a [Reactive Core](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0002-reactive-core.md)
3. It exposes a [Duplex Asynchronous API](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0003-asynchronous-duplex-api.md) using WebSockets protocol
4. We use the [Handle pattern](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0004-use-handle-to-model-effects.md) to "model" effectful computations
5. We use [IOSim](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0005-use-io-sim.md) for all concurrency primitives
6. Network [broadcasts all messages](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0006-network-broadcasts-all-messages.md)
7. _Components are defined using [with-pattern](https://github.com/input-output-hk/hydra-poc/blob/abailly-iohk/introduce-adrs/docs/adr/0007-with-pattern-for-duplex-channels.md)_
