# 6. Network Broadcasts all messages

Date: 2021-06-08

## Status

Accepted

## Context

* Hydra nodes always _broadcast_ messages over a Hydra network, expecting each message to reach every node without having explicit addressing
* While unicast or multicast appear to be a better fit and less wasteful of network resources, and is how message sendind in the network is modelled in the [original Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/), systematic broadcasting has several advantages:
  * It does not seem to save much messages as broadcasting `AckTx` messages removes the need to send a `ConfTx`: Each node can confirm transactions on its own
  * Coupled with a fully-connected network topology, it simplifies messages handling and routing: Simply send everything to everyone and handle any message received
  * It makes it simpler to build non-fully connected networks, and more generally accomodate for various network topologies: Nodes act both as destinary of messages and relays to propagate messages over the network until they reach all interested parties.
    > **NOTE**: This is directly inspired by how transactions are propagated between cardano-node using ouroboros [TxSubmission](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network/src/Ouroboros/Network/TxSubmission) protocol
    
## Decision

* All messages emitted by a Hydra node through a `NetworkEffect` are _broadcasted_ to _all_ nodes in the network
* This implies the emitter shall itself receive the message as a `NetworkEvent`

## Consequences

* The network layer is responsible for ensuring sent messages effectively reaches all nodes in the network. How this is achieved is left as an implementation detail

