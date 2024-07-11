# Architecture

This document describes the architecture of the current implementation
of a `hydra-node`. The following picture represents the main
components of a Hydra node using the [C4 Component
Diagram](https://c4model.com/#ComponentDiagram) notation.

```mdx-code-block
import ArchitectureSvg from './hydra-components.svg';

<ArchitectureSvg className="architecture" />
```

:::info

This diagram is manually produced using [PlantUML](https://plantuml.com) graphing tool with [C4 extensions](https://github.com/plantuml-stdlib/C4-PlantUML).

```
$ plantuml -Tsvg architecture-c4.puml
```

:::

### Network

_Network_ component is responsible for all of the communications related to the off-chain part of the Hydra protocol between hydra-nodes. The [current implementation](./networking) is based on the [Typed Protocols](https://github.com/input-output-hk/typed-protocols) library which is also at the heart of the Cardano node's networking. It's asynchronous by nature and is using a push based protocol with a uniform _broadcast_ abstraction. 

Messages are exchanged between nodes on different internal transitions and are authenticated using each peer _Hydra Key_: Each message sent is signed by the emitter and the signature is verified by the transmitter.

##### Authentication & Authorization

The messages exchanged through the _Hydra Network_ layer between
participants are authenticated: Each message is
[signed](https://github.com/cardano-scaling/hydra/issues/727) using
the Hydra signing key of the emitting party which is identified by
the corresponding verification key. When a message with an unknown
or incorrect signature is received, it is dropped and a notification
is logged.

Messages are however not encrypted and therefore, if confidentiality is
required, some external mechanism needs to be put in place to prevent
other parties from observing the messages exchanged within a Head.

##### Fault tolerance

The Hydra Head protocol guarantees safety of all (honest)
participants' funds, but does not in itself guarantee liveness, hence
all parties involved in a Hydra Head must be online and reactive for
the protocol to make progress.

This implies that, should one or several participants' hydra-node
become _permanently_ unreachable from the others, through a crash, or
a network partition, no more transactions can happen in the Head and
it must be closed. However, the [Hydra network
layer](https://hydra.family/head-protocol/unstable/haddock/hydra-node/Hydra-Node-Network.html)
is tolerant to _transient_ disconnections and (non-Byzantine) crashes.

### Chain Interaction

#### Chain

The _Chain_ component is responsible for interfacing the Hydra node with the Cardano (aka. Layer 1) chain. The current, so-called `Direct`, implementation uses the `Node-to-Client` protocol to both "follow the chain" and observe new blocks and transactions which can change the state of the head, and submit transactions in response to client's requests or as needed to advance the protocol's state. It connects to a locally spun cardano-node using the _local socket_and contains the \_off-chain_ logic, based on cardano-api, that knows how to observe and build transactions relevant to the Hydra protocol. See [ADR-10](/adr/10) for more details.

#### Wallet

The Hydra node maintains an internal wallet using the Cardano signing key provided to the `hydra-node`. This is used to handle the payment of transaction fees and signing transactions.

### Head Logic

This is the component which is the heart of the Hydra node, implementing the protocol's _state machine_. It is structured around the concepts of `Input`s and `Effect`s: `Input`s from the outside world are interpreted against the current state, this may result in internal `Event`s, which are aggregated into an updated state, and `Effect`s which result in "side-effects" on the outside world. The state available to the _Head Logic_ consists of both, the content of the Head itself (eg. current Ledger, transactions pending) _and_ the data from the Layer 1 that's needed to observe and trigger on-chain transitions.

### Hydra Smart Contracts

This "component" represents all of the Hydra smart contracts needed for Head protocol operation. Currently the contracts are written using `Plutus-Tx`. The scripts are optimized using custom `ScriptContext` and error codes for now.

### API

`hydra-node` exposes an [Asynchronous API](https://hydra.family/head-protocol/unstable/api-reference) through a Websocket server. This API is available to _Hydra Client_ to send commands and observe changes in the state of the Hydra head. Upon startup, the API server loads all historical messages from persistence layer and serves them to clients in case they are interested in observing them.

### Persistence

All API server outputs and the `hydra-node` state is preserved on disk. The persistence layer is responsible for loading the historical messages/hydra state from disk and also storing them. For the time being there was no need to make this layer more complex or use a database.

### Logging

The Hydra node logs all side-effects occurring internally as JSON-formatted messages to the _standard output_ stream attached to its process. The format of the logs is documented as a [JSON schema](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/json-schemas/logs.yaml), and follows the principles outlined in [ADR-9](/adr/9).

### Monitoring

The Hydra node [optionally](https://hydra.family/head-protocol/docs/getting-started/quickstart#hydra-node-options) exposes [Prometheus](https://prometheus.io/)-compliant _metrics_ through an HTTP server, on the standard `/metrics` endpoint.
