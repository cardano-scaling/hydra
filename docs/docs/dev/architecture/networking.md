# Networking

This document provides details about the Hydra networking layer, which encompasses the network of Hydra nodes where heads can be opened.

:::warning

🛠 This document is a work in progress. We recognize that the current state of networking is suboptimal, serving as an initial implementation to establish a functional basis. Efforts are underway to enhance the network dynamics through a proposed improvement initiative, detailed in [this proposal](https://github.com/input-output-hk/hydra/pull/237).
:::

## Questions

- What's the expected topology of the transport layer?
  - Are connected peers a subset, superset, or identical set of the head parties?
- Do we need the delivery ordering and reliability guarantees TCP provides?
  - TCP provides full-duplex, stream-oriented, persistent connections between nodes
  - The Hydra networking layer is based on asynchronous message passing, which seems better suited to UDP
- Do we need to consider nodes being reachable through firewalls?
  - This responsibility could be delegated to end users, allowing them to configure their firewalls/NATs to align with Hydra node requirements
  - This may be more manageable for business, corporate, or organizational parties than for individual end-users
- Do we want _privacy_ within a head?
  - Transactions' details should be opaque to outside observers, with only the final outcome of the head's fanout being observable
- How do we identify/discover peers/parties?
  - The paper assumes a _setup_ phase where:
    > To create a head-protocol instance, an initiator invites a set of participants \{p1,...,pn\} (including themselves) to join by announcing protocol parameters: the participant list, parameters of the (multi-)signature scheme, etc.
    > Each party subsequently establishes pairwise authenticated channels with all other parties involved.
- What constitutes a _list of participants_? Should each participant be uniquely identifiable? If so, what identification method should be used — naming scheme, IP: port address, public key, certificate?
  - What do 'pairwise authenticated channels' entail? Are these actual TCP/TLS connections, or do they operate at the Transport (layer 4) or Session (layer 5) level?
- How open do we want our network protocol to be?
  - Currently leveraging the Ouroboros stack with CBOR message encoding, integrating other tools into the Hydra network may pose challenges.

## Investigations

### Ouroboros

We held a meeting with the networking team on February 14, 2022, to explore the integration of the Ouroboros network stack into Hydra. During the discussion, there was a notable focus on performance, with Neil Davies providing insightful performance metrics.

- World circumference: 600ms
- Latency w/in 1 continent: 50-100ms
- Latency w/in DC: 2-3ms
- Subsecond roundtrip should be fine wherever the nodes are located
- Basic reliability of TCP connections decreases w/ distance:
  - w/in DC connection can last forever
  - outside DC: it's hard to keep a single TCP cnx up forever; if a reroute occurs because some intermediate node is down, it takes 90s to resettle a route
  - this implies that as the number of connections goes up, the probability of having at least one connection down at all times increases
- Closing of the head must be dissociated from network connections => a TCP cnx disappearing =/=> closing the head
- Within the Cardano network, propagation of a single empty block takes 400ms (to reach 10K nodes)
  - the Ouroboros network should withstand 1000s of connections (there are some system-level limits)
- Modelling the Hydra network
  - a logical framework for modelling the performance of network associate CDF with time for a message to appear at all nodes (this is what is done in the [hydra-sim](https://github.com/input-output-hk/hydra-sim)
  - we could define a layer w/ the semantics we expect; for example, Snocket = PTP connection w/ ordered guaranteed messages delivery – do we need that in Hydra?
- How about [Wireguard](https://wireguard.io)? It's a very interesting approach, with some shortcomings:
  - no global addressing scheme
  - there is one `eth` interface/connection
  - on the plus side, it transparently manages IP address changes
  - does not help w/ Firewalls, eg NAT needs to be configured on each node.

### Cardano networking

See [this Wiki page](https://github.com/input-output-hk/hydra.wiki/blob/master/Networking.md#L1) for detailed notes about how the Cardano network works and uses Ouroboros.

- Cardano is a global network spanning thousands of nodes, with nodes constantly joining and leaving, resulting in a widely varying topology. Its primary function is block propagation: blocks produced by certain nodes according to consensus rules must reach every node in the network within 20 seconds.
- Nodes cannot maintain direct connections to all other nodes; instead, block diffusion occurs through a form of _gossiping_. Each node is connected to a limited set of peers with whom it exchanges blocks.
- Nodes must withstand adversarial behavior from peers and other nodes, necessitating control over the amount and rate of data they ingest. Hence, a _pull-based_ messaging layer is essential.
- Producer nodes, which require access to signing keys, are considered sensitive assets. They are typically operated behind *relay nodes* to enhance security and mitigate the risks of DoS attacks or other malicious activities.
- Nodes often operate behind ADSL or cable modems, firewalls, or in other complex networking environments that prevent direct addressing. Therefore, nodes must initiate connections to externally reachable *relay nodes*, and rely on a *pull-based* messaging approach.

## Implementations

### Current state

- Hydra nodes form a network of pairwise connected *peers* using point-to-point (eg, TCP) connections that are expected to remain active at all times:
  - Nodes use [Ouroboros](https://github.com/input-output-hk/ouroboros-network/) as the underlying network abstraction, which manages connections with peers via a reliable point-to-point stream-based communication framework known as a `Snocket`
  - All messages are _broadcast_ to peers using the PTP connections
  - Due to the nature of the Hydra protocol, the lack of a connection to a peer halts any progress of the head.
- A `hydra-node` can only open a head with *all* its peers and exclusively with them. This necessitates that nodes possess prior knowledge of the topology of both peers and heads they intend to establish.
- Connected nodes implement basic _failure detection_ through heartbeats and monitoring exchanged messages.
- Messages exchanged between peers are signed using the party's Hydra key and validated upon receiving.

### Gossip diffusion network

The following diagram illustrates one possible implementation of a pull-based messaging system for Hydra, developed from discussions with IOG’s networking engineers:

![Hydra pull-based network](./hydra-pull-based-network.jpg)
