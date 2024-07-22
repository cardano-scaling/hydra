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

### Network

The _network_ component is responsible for all communications between Hydra nodes related to the off-chain part of the Hydra protocol. The [current implementation](./networking) is based on the [typed protocols](https://github.com/input-output-hk/typed-protocols) library, which is also used by the Cardano networking. It is asynchronous by nature and uses a push-based protocol with a uniform _broadcast_ abstraction.

Messages are exchanged between nodes during different internal transitions and are authenticated using each peer's _Hydra key_. Each message sent is signed by the sender, and the signature is verified by the receiver.

#### Authentication and authorization

The messages exchanged through the _Hydra networking_ layer between participants are authenticated. Each message is [signed](https://github.com/input-output-hk/hydra/issues/727) using the Hydra signing key of the emitting party, which is identified by the corresponding verification key. When a message with an unknown or incorrect signature is received, it is dropped, and a notification is logged.

However, messages are not encrypted. If confidentiality is required, an external mechanism must be implemented to prevent other parties from observing the messages exchanged within a head.

#### Fault tolerance

The Hydra Head protocol guarantees the safety of all honest participants' funds but does not inherently guarantee liveness. Therefore, for the protocol to progress, all parties involved in a head must be online and reactive.

This means that if one or more participants' Hydra nodes become permanently unreachable due to a crash or network partition, no further transactions can occur in the head, and it must be closed. However, the [Hydra networking layer](https://hydra.family/head-protocol/unstable/haddock/hydra-node/Hydra-Node-Network.html) is tolerant to transient disconnections and (non-Byzantine) crashes.

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

All API server outputs and the `hydra-node` state are preserved on disk. The persistence layer is responsible for loading historical messages and the Hydra state from disk, as well as storing them. Currently, there hasn't been a need to increase the complexity of this layer or use a database.

### Logging

The Hydra node logs all internal side effects as JSON-formatted messages to its _standard output_ stream. The log format adheres to a documented [JSON schema](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/json-schemas/logs.yaml), following the principles outlined in [ADR-9](/adr/9).

### Monitoring

The Hydra node [optionally](https://hydra.family/head-protocol/docs/getting-started/quickstart#hydra-node-options) exposes [Prometheus](https://prometheus.io/)-compliant _metrics_ through an HTTP server, on the standard `/metrics` endpoint.
