---
sidebar_position: 2
---

# Hydra Node Architecture

This document describes the architecture of the current implementation
of a `hydra-node`. The following picture represents the main
components of a Hydra node using the [C4 Component
Diagram](https://c4model.com/#ComponentDiagram) notation.

```mdx-code-block
import ArchitectureSvg from './hydra-components.svg';

<ArchitectureSvg className="architecture" />
```


### Network

_Network_ component is responsible for all of the communications related to the off-chain part of the Hydra protocol between hydra-nodes. The [current implementation](./networking) is based on the [Typed Protocols](https://github.com/input-output-hk/typed-protocols) library which is also at the heart of the Cardano node's networking. It's asynchronous by nature and using push based protocol. Messages are exchanged between nodes on each event and currently there is no message authentication in place. We rely on a fact that we know each peers IP address and a port and _trust_ these messages blindly. There are [plans](https://github.com/input-output-hk/hydra/issues/727) on introducing message signing to improve on security properties of the hydra protocol.

### Chain Interaction

#### Direct Chain

The _Chain_ component is responsible for interfacing the Hydra node with the Cardano (aka. Layer 1) chain. It uses the `Node-to-Client` protocol to both "follow the chain" and observe new blocks and transactions which can change the state of the head, and submit transactions in response to client's requests or as needed to advance the protocol's state. It connects to a locally spun cardano-node using the _local socket_and contains the _off-chain_ logic, based on cardano-api, that knows how to observe and build transactions relevant to the Hydra protocol.

:::info

The reason why this component is called _Direct_ chain is historical: Initially, we thought of using the [Plutus Application Backend](https://plutus-apps.readthedocs.io/en/latest/plutus/explanations/pab.html) to handle interaction between the off- and on-chain parts of the Hydra protocol but we switched to use the lower level node-to-client protocol.

:::

#### Tiny Wallet

Hydra also maintains an internal "wallet" which is currently part of the Chain components layer, to handle the payment of transaction fees and signing transctions using a dedicated Cardano signing key. There are [plans](https://github.com/input-output-hk/hydra/issues/215) to make it possible to use an external  wallet to balance and sign Hydra node's transactions.

### Head Logic

This is the component which is the heart of the Hydra node, implementing the protocol's _input-output state machine_. It is structured around the concepts of `Event`s and `Effect`s:

* `Event`s are _inputs_ to the state machine from various parts of the node that can change the state and they ...
* ... produce `Effect`s which are _outputs_ from the state machine interpreted by other components to produce "side-effects".

The _Head Logic_ of course maintains the internal state of the head and persists it when it changes. This state consists in both the content of the Head itself (eg. current Ledger, transactions pending) _and_ the data from the Layer 1 that's needed to observe and trigger on-chain transitions.

### Hydra Smart Contracts

This "component" represents  all of the Hydra smart contracts needed for Head protocol operation. Currently the contracts are written using `Plutus-Tx`. The scripts are optimized using custom `ScriptContext` and error codes for now.

### API layer

`hydra-node` exposes an [Asynchronous API](https://hydra.family/head-protocol/unstable/api-reference) through a Websocket server. This API is available to _Hydra Client_ to send commands and observe changes in the state of the Hydra head. Upon startup, the API server loads all historical messages from persistence layer and serves them to clients in case they are interested in observing them.

### Persistence

All API server outputs and the `hydra-node` state is preserved on disk. The persistence layer is responsible for loading the historical messages/hydra state from disk and also storing them. For the time being there was no need to make this layer more complex or use a database.

### Logging

The Hydra node logs all side-effects occuring internally as JSON-formatted messages to the _standard output_ stream attached to its process. The format of the logs is documented as a [JSON schema](https://raw.githubusercontent.com/input-output-hk/hydra/master/hydra-node/json-schemas/logs.yaml), and follows the principles outlined in [ADR-9](/adr/9).

### Monitoring

The Hydra node [optionally](http://localhost:3000/head-protocol/docs/getting-started/quickstart#hydra-node-options) exposes [Prometheus](https://prometheus.io/)-compliant _metrics_ through an HTTP server, on the standard `/metrics` endpoint.
