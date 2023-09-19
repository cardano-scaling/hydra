---
slug: 26
title: |
  26. Network failures model
authors: []
tags: [Proposed]
---

## Status

Proposed

## Context

The current Head cluster is very fragile as has been observed on several occasions: A single hiccup in the connectivity between nodes while a head is open and nodes are exchanging messages can very easily lead to the Head being stuck and require an emergency closing, possibly even manually.

We want Hydra to be _Consistent_ in the presence of _Network Partitions_, under the _fail-recovery_ model assumption, eg. processes may fail by stopping and later recovering. Our system lies in the [CP](https://en.wikipedia.org/wiki/CAP_theorem) space of the landscape mapped by the CAP theorem.

We have identified 3 main sources of failures in the _fail-recovery_ model that can lead to a head being stuck:

1. The network layer can drop messages from the moment a node `broadcast`s it, leading to some messages not being received at the other end
2. The sending node can crash in between the moment the state is changed (and persisted) and the moment a message is actually sent through the network (or even it calls `broadcast`)
3. The receiving node can crash in between the moment the message has been received in the network layer, and it's processed (goes through the queue)

We agree that we'll want to address all those issues in order to provide a good user experience, as not addressing 2. and 3. can lead to hard to troubleshoot issues with heads. We have not experienced those issues yet as they would probably only crop up under heavy loads, or in the wild. But we also agree we want to tackle 1. first because it's where most of the risk lies. By providing a _Reliable Broadcast_ layer, we will significantly reduce the risks and can then later on address the other points.

Therefore, the scope of this ADR is to address only point 1. above: Ensure broadcast messages are eventually received by all peers, given the sender does not stop before.

### Discussion

* We are currently using the [ouroboros-framework](https://github.com/input-output-hk/ouroboros-network) and [typed-protocols](https://github.com/input-output-hk/typed-protocols) network stack as a mere [transport](https://osi-model.com/transport-layer/) layer.
  * Being built on top of TCP, ouroboros multiplexer (Mux) provides the same reliability guarantees, plus the multiplexing capabilities of course
  * It also takes care of reconnecting to peers when a failure is detected which relieves us from doing so, but any reconnection implies a reset of each peer's state machine which means we need to make sure any change to the state of pending/received messages is handled by the applicative layer
  * Our [FireForget](https://github.com/input-output-hk/hydra/blob/8a8e0829964132bde8949e5249a1ab303af92fb8/hydra-node/src/Hydra/Network/Ouroboros/Type.hs#L31) ignores connections/disconnections
  * Ouroboros/typed-protocols provides enough machinery to implement a reliable broadcast protocol, for example by reusing existing `KeepAlive` protocol and building a more robust point-to-point protocol than what we have now
  * There is a minor limitation, namely that the subscription mechanism does not handle connections invidually, but as a set of equivalent point-to-point full duplex connections whose size (valency) needs to be maintained at a certain threshold, which means that unless backed in the protocol itself, protocol state-machine and applications are not aware of the identity of the remote peer
* We have built our `Network` infrastructure over the concept of relatively independent layers, each implementing a similar interface with different kind of messages, to `broadcast` messages to all peers and be notified of incoming messages through a `callback`.
  * This pipes-like abstraction allows us to compose our network stack like:

    ```
     withAuthentication (contramap Authentication tracer) signingKey otherParties $
      withHeartbeat nodeId connectionMessages $
        withOuroborosNetwork (contramap Network tracer) localhost peers
    ```

  * This has the nice property that we can basically swap the lower layers should we need to, for example to use [UDP](https://github.com/input-output-hk/hydra/blob/abailly-iohk/multi-node-udp/hydra-node/src/Hydra/Network/UDP.hs), or add other layers for example to identify [Multiple Heads](https://github.com/input-output-hk/hydra/blob/abailly-iohk/multi-node-udp/hydra-node/src/Hydra/Network/MultiHead.hs#L26)

## Decision

* We implement our own message tracking and resending logic as a standalone `Network` layer
* That layer consumes and produces `Authenticate`d messages in order to identify the source of messages
* It uses a vector of monotonically increasing _sequence numbers_ associated with each party (including itself) to track what are the last messages from each party and to ensure FIFO delivery of messages
* This _vector_ is also used to identify peers which are lagging behind and to resend the missing messages, or to drop messages which have already been received
* This _vector_ is also piggybacked by the ping messages of the heartbeat mechanism so that a node informs its peers of its own situation even if it would have, otherwise, no message to sent to them
* Sending a ping message does not influence the _vector_ of the sender
* Any message received which index does not match what the peer expects is dropped
* Messages deemed not received by a peer a re-sent

## Consequences

* We keep our existing `Network` interface hence all messages will be resent to all peers
  * This could be later optimized either by providing a smarter interface with a `send :: Peer -> msg -> m ()` unicast function, or by adding a layer with filtering capabilities, or both
* We need to ensure messages are not kept forever when resending, eg. that the pending messages list is garbage collected
* We need to refactor our `Heartbeat` layer to decouple the 2 capabilities it embeds, namely sending periodical heartbeats to peers, and providing listeners with information about a peer connection status, so that the `Reliability` layer can actually rely on those heartbeats to get regular update of peers' knowledge even when the actual Head is idle.
* We want to specify this protocol clearly in order to ease implementation in other languages, detailing the structure of messages and the semantics of retries and timeouts.
* We do not implement a _pull-based_ message communication mechanism as initially envisioned
* We do not persist messages either on the receiving or sending side at this time
* We may consider relying on the vector clock in the future to ensure perfect ordering of messages on each peer and make impossible for legit transactions to be temporarily seen as invalid. This can happen in the current version and is handled through wait and ttl
