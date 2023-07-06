---
sidebar_position: 5
---

# Hydra Networking

This document provides details about the _Hydra Networking Layer_, eg. the network comprised of Hydra nodes upon which Heads can be opened.

:::warning

🛠 This document is a work in progress. We know the current situation w.r.t. networking is less than ideal, it's just a way to get started and have _something_ that works. There is already a [proposal](https://github.com/input-output-hk/hydra/pull/237) to improve the situation by making the network more dynamic.
:::

# Questions

- What's the expected topology of the transport layer?
  - Are connected peers a subset/superset/identity set of the Head parties?
- Do we need the delivery ordering and reliability guarantees TCP provides?
  - TCP is full-duplex stream oriented persistent connection between nodes
  - Our networking layer is based on asynchronous messages passing which seems better suited to UDP
- Do we need to care about nodes being reachable through firewalls?
  - This could be something we push onto end users, eg. let them be responsible of configuring their firewalls/NATs to match Hydra node needs
  - Probably easier for business/corporate/organisation players than for end-users
- Do we want _privacy_ within a Head?
  - The details of txs should be opaque for outside observers, only the end result of the Head's fanout is observable
- How do we know/discover peers/parties?
  - The paper assumes there exists a _Setup_ phase where
    > In order to create a head-protocol instance, an initiator invites a set of participants ${p1,...,pn}$ (himself being one of them) to join by announcing to them the protocol parameters: the list of participants, the parameters of the (multi-)signature scheme to be used, etc.
    > Each party then establishes pairwise authenticated channels to all other parties.
  - What exactly is a _list of participants_? It seems at the very least each participant should be _identified_, in order to be distinguished from each other, but how? Some naming scheme? IP:Port address? Public key? Certificate?
  - What are "pairwise authenticated channels" exactly? Are these actual TCP/TLS connections? Or is it more a layer 4 (Transport) or layer 5 (Session) solution?
- How open do we want our network protocol to be?
  - We are currently using Ouroboros stack with CBOR encoding of messages, this will make it somewhat hard to have other tools be part of the Hydra network

# Investigations

## Ouroboros

We had a meeting with network team on 2022-02-14 where we investigated how Ouroboros network stack fits in Hydra.
Discussion quickly derived on performance, with Neil Davies giving some interesting numbers:

- World circumference: 600ms
- Latency w/in 1 continent: 50-100ms
- Latency w/in DC: 2-3ms
- Subsecond roundtrip should be fine wherever the nodes are located
- basic reliability of TCP connections decrease w/ distance:
  - w/in DC connection can last forever)
  - outside DC: it's hard to keep a single TCP cnx up forever, if a reroute occurs because some intermediate node is down, it takes 90s to resettle a route
  - This implies that as the number of connections goes up, the probability of having at least one connection down at all time increases
- closing of the head must be dissociated from network connections => a TCP cnx disappearing =/=> closing the head
- Within cardano network, propagation of a single empty block takes 400ms (to reach 10K nodes)
  - ouroboros network should withstand 1000s of connections (there are some system-level limits)
- Modelling Hydra network
  - A logical framework for modelling performance of network associate CDF with time for a message to appear at all nodes (this is what is done in the [hydra-sim](https://github.com/input-output-hk/hydra-sim)
  - We could define a layer w/ the semantics we expect, eg. Snocket = PTP connection w/ ordered guaranteed messages delivery. Do we need that in Hydra?
- How about [Wireguard](https://wireguard.io)? It's a very interesting approach, with some shortcomings:
  - no global addressing scheme
  - there is one `eth` interface / connection
  - on the plus side, it transparently manages IP address changes
  - does not help w/ Firewalls, eg. NAT needs to be configured on each node

## Cardano Networking

See [this wiki page](https://github.com/input-output-hk/hydra.wiki/blob/master/Networking.md#L1) for detailed notes about how Cardano network works and uses Ouroboros.

- Cardano is a global network spanning 1000s of nodes, with nodes coming and going and a widely varying topology. Its main purpose is _block propagation_: Blocks produced by some nodes according to the consensus rules needs to reach every node in the network in less than 20 seconds.
- Nodes cannot be connected to all other nodes, as such block diffusion occurs through some form of _gossipping_ whereby a node is connected to a limited set of peers with which it exchanges blocks
- Nodes need to be resilient to adversarial behaviour from peers or other nodes, as such they need to control the amount and rate of data they want to ingest, hence the need for a _pull-based_ messaging layer
- Producer nodes are sensitive assets as they need access to signing keys, hence are usually run behind _relay nodes_ in order to increase safety and reduce the risk of DoS or other malicious activities
- Nodes are expected to run behind ADSL or cable boxes, firewalls, or in other complex networking settings preventing nodes to be _addressed_ directly, hence the need for nodes to _initiate_ connections to externally reachable _relay nodes_, and for _pull-based_ messaging

# Implementations

## Current State

- Hydra nodes form a network of pairwise connected _peers_ using Point-to-point (e.g TCP) connections. Those connections are expected to be up and running at all time
  - Nodes use [Ouroboros](https://github.com/input-output-hk/ouroboros-network/) as the underlying network abstraction, which takes care of managing connections with peers providing a reliable Point-to-Point stream-based communication abstraction called a `Snocket`
  - All messages are _broadcast_ to peers using the PTP connections
  - Due to the nature of the Hydra protocol, the lack of connection to a peer prevents any progress of the Head
- A `hydra-node` can only open a Head with _all_ its peers, and only them. This implies the nodes need to know in advance the topology of the peers and Heads they want to open
- Connected nodes implement basic _failure detection_ through heartbeats and monitoring exchanged messages
- Messages between peers are signed using party's hydra key and also validated upon receiving.

## Gossip diffusion network

The following diagram is one possible implementation of a pull-based messaging system for Hydra, drawn from a discussion with IOG's networking engineers:

![Hydra pull-based network](./hydra-pull-based-network.jpg)
