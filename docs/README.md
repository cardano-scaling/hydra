# Hydra Node Technical Architecture

This document is essentially a collection of links to other places in the source repository

## Principles

We use _Architecture Decision Records (ADR)_ for a lightweight technical
documentation about our principles and significant design decisions. The
architecture itself then is just a result of all accepted ADRs, which have not
been deprecated or superseeded. An up-to-date index of still relevant ADRs is
kept [here](./adr/README.md).

## Overview

The following diagram represents the internal structure of the Hydra Node and the interactions between its components.

![](images/hydra-components.jpg)

**Legend**:
- Grayed boxes represent components which are not developed yet
- Black boxes represent components which are expected to be used as _black box_, eg. without any knowledge of their inner workings.
- Arrows depict the flow of data (Requests, messages, responses...)
- We represent some components that are not part of the Hydra node proper for legibility's sake
- TODO: explain colored borders (test boundaries) or remove them

## Components

> **TODO**: Move as haddock comments to each module/function

Please refer to each component's internal documentation for details.

* The [HydraNode](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Node.hs) is a handle to all other components' handles
  * This handle is used by the main loop to `processNextEvent` and `processEffect`
* The [HeadLogic](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/HeadLogic.hs) component implements the Head Protocol's _state machine_ as a _pure function_.
  * The protocol is described in two parts in the [Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/):
    * One part detailing how the Head deals with _clients input_, eg. [ClientRequest](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/HeadLogic.hs#L43):
    * Another part detailing how the Head reacts to _peers input_ provided by the network, eg. [HydraMessage](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/HeadLogic.hs#L78):
* The [OnChain](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Node.hs#L154) client implements the _Head-Chain Interaction_ part of the protocol
  * Incoming and outgoing on-chain transactions are modelled as an [OnChainTx](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/HeadLogic.hs#L88) data type that abstracts away the details of the structure of the transaction.
  * In order to ease the development process, we provide an idealised version of a blockchain client, implemented using [0MQ](https://zeromq.org/).
    * The server is implemented as a standalone [executable](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/exe/hydra-node/Main.hs) which simply stores and forwards all transactions received to all connected clients using Pub/Sub connections.
    * As clients can come and go at any time, the server also provides a `Rep` type socket for clients to request [past transactions](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Network/ZeroMQ.hs#L62)
    * The Hydra node [catch-up](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Network/ZeroMQ.hs#L51) with past transactions when it [starts the On-Chain client](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/exe/hydra-node/Main.hs#L42).
  * A PAB Chain Client is planned for development but is not currently implemented, see the _Chain Client and Smart Contracts_ section for more details.
* The [Network](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Network.hs) component provides the Node an asynchronous messaging interface to the Hydra Network, e.g to other Hydra nodes
  * Incoming and outgoing messages are modelled as [HydraMessage](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/HeadLogic.hs#L78) data type
  * We provide 2 implementations of the network, one based on the [ouroboros-network](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network-framework) framework and one based on [0MQ](https://zeromq.org/)
    * The [Ouroboros](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Network/Ouroboros.hs) based network layer implements a dumb [FireForget](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Network/Ouroboros/Type.hs#L27) protocol. Contrary to other protocols implemented in Ouroboros, this is a push-based protocol
    * The [0MQ](../hydra-node/src/Hydra/Network/ZeroMQ.hs) based network
* The main constituent of the Head's state is the [Ledger](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Ledger.hs) which allows the head to maintain and update the state of _Seen_ or _Confirmed_ transactions and UTxOs according to its protocol.
  * There is a [MockTx](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Ledger/Mock.hs) ledger which ignores the nitty-gritty details of how transactions are defined and is only interested in whether or not they are _valid_
  * [MaryTest](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Ledger/MaryTest.hs) provides a more concrete implementation based on a Mary-era Shelley ledger, but with test cryptographic routines
* Structured logging is implemented using [IOHK monitoring framework](https://github.com/input-output-hk/iohk-monitoring-framework) which provides backend for [contra-tracer](https://hackage.haskell.org/package/contra-tracer) generic logging
  * Each component defines its own tracing messages as a datatype and they are aggregated in the [HydraLog](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Logging/Messages.hs) datatype. Specialized `Tracer`s can be passed around from the top-level one using `contramap` to peel one layer of the onion
  * Configuration of the main tracer is done via the [withTracer](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Logging.hs) wrapper function
* Metrics and monitoring are piggy-backed on tracing events:
  * Monitoring collection is configured at start of the hydra-node
  * Traced events are [interpreted](https://github.com/input-output-hk/hydra-poc/blob/d24c04e138acd333c3d47f97bb214957785fde08/hydra-node/src/Hydra/Logging/Monitoring.hs) as contributing to some specific metric value without trace producers needing to be aware of how this process happens
  * Metrics are exposed using [Prometheus](https://prometheus.io/docs/instrumenting/exposition_formats/) format over URI `/metrics` from an HTTP server started on a configurable port.
