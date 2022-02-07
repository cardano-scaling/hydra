# Architecture Decision Records

Here is a list of currently accepted or discussed **Architecture Decision Records**, which serve as a lightweight technical documentation for the Hydra node software. As new ADRs are added and old ones are deprecated or superseded, this list should be updated to reflect the current situation. Follow the links to have a detailed explanation for each ADR.

1. Principles and patterns are documented using [Architecture Decision Records](./0001-record-architecture-decisions.md)
2. Hydra Node is designed around a [Reactive Core](./0002-reactive-core.md)
3. It exposes a [Duplex Asynchronous API](./0003-asynchronous-duplex-api.md) using WebSockets protocol
4. We use the [Handle pattern](./0004-use-handle-to-model-effects.md) for effectful computations
5. We use [io-classes](./0005-use-io-sim-classes.md) for all abstracting concurrency primitives
6. Network [broadcasts all messages](./0006-network-broadcasts-all-messages.md)
7. _Components are defined using [with-pattern](./0007-with-pattern-component-interfaces.md)_
8. [Use custom Prelude](./0008-use-custom-prelude.md)
9. [Simplify logging](./0009-simplify-logging.md)
10. Use [Direct chain](./0010-use-direct-chain.md) connection
11. _Standardize on Cardano API_
12. [Top-down test-driven design](./0012-testing-strategy.md)
13. [Adversarial Mutation-based Contract Testing](./0013-contract-testing-strategy.md)
14. [Token usage in Hydra scripts](./0014-hydra-script-tokens.md)
