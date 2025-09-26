# Architecture

This document describes the architecture of the current implementation
of a `hydra-node`. The following diagram represents the main
components of a Hydra node using the [C4 Component
Diagram](https://c4model.com/#ComponentDiagram) notation.

```mdx-code-block
import ArchitectureSvg from './hydra-components.svg';

<ArchitectureSvg className="architecture" />
```

:::info

This diagram is manually produced using the [PlantUML](https://plantuml.com) graphing tool with [C4 extensions](https://github.com/plantuml-stdlib/C4-PlantUML).

```
$ plantuml -Tsvg architecture-c4.puml
```

:::

### Event sourcing

The `hydra-node` is an event sourced application. This means that the main logic is processing _inputs_ (also called commands) and produces _events_. These events are saved and loaded to persist application state across restarts. Also, most events are transformed to _outputs_ and can be observed on the API. See [Event Sourcing](./architecture/event-sourcing) for details.

### Network

The _network_ component is responsible for communication between Hydra nodes related to the off-chain part of the Hydra protocol. See [Networking](./architecture/networking) for details.

### Chain interaction

#### Chain

The _chain_ component interfaces the Hydra node with the Cardano (layer 1) chain. The current implementation, called `Direct`, uses the `node-to-client` protocol to both 'follow the chain' and observe new blocks and transactions that can change the state of the head. It also submits transactions in response to client requests or as needed to advance the protocol's state. This component connects to a locally spun `cardano-node` using the _local socket_ and contains the _off-chain_ logic, based on `cardano-api`, which knows how to observe and build transactions relevant to the Hydra protocol. For more details, see [ADR-10](/adr/10).

#### Wallet

The Hydra node maintains an internal wallet using the Cardano signing key provided to the `hydra-node`. This wallet is used to handle the payment of transaction fees and to sign transactions.

### Head logic

This component is at the core of the Hydra node, implementing the protocol's _state machine_. It is structured around the concepts of `Input`s and `Effect`s. `Input`s from the outside world are interpreted against the current state, potentially resulting in internal `Event`s, which are aggregated into an updated state. This can then produce `Effect`s, leading to 'side effects' on the outside world. The state available to the _head logic_ consists of the head's content (eg, current ledger, pending transactions) _and_ the data from layer 1 needed to observe and trigger on-chain transitions.

### Hydra smart contracts

This component represents all the Hydra smart contracts needed for the Hydra Head protocol operation. Currently, the contracts are written using `Plutus-Tx`. The scripts are optimized using a custom `ScriptContext` and specific error codes.

### API

The `hydra-node` component exposes an [asynchronous API](https://hydra.family/head-protocol/unstable/api-reference) through a WebSocket server. This API is available to the _Hydra client_ for sending commands and observing changes in the state of the Hydra head. Upon startup, the API server loads all historical messages from the persistence layer and serves them to clients interested in observing them.

### Persistence

The `hydra-node` state is preserved on disk. The persistence layer is responsible for loading historical messages and Hydra state from disk, as well as storing them in so-called event log files. Depending on the rotation configuration used at startup, these event log files will be rotated to improve restart times. So far, there hasn’t been a need to increase the complexity of this layer or to use a database.

### Logging

The Hydra node logs all internal side effects as JSON-formatted messages to its _standard output_ stream.

### Monitoring

The Hydra node [optionally](https://hydra.family/head-protocol/docs/getting-started#monitoring) exposes [Prometheus](https://prometheus.io/)-compliant _metrics_ through an HTTP server, on the standard `/metrics` endpoint.
