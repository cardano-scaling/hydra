# 6. Network Broadcasts all messages

Date: 2021-06-08

## Status

Accepted

## Context

The simplified Head protocol in the [Hydra
paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/)
requires _unicast_ and _multicast_ messaging between participants. However, this
can be simplified to only _multicast_ by also sending `AckTx` messages to all
participants and removing the necessity for `ConfTx`.

There is already a battle-tested implementation for _broadcasting_ messages over
networks with any kind of topology (mesh), namely the
[TxSubmission](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network/src/Ouroboros/Network/TxSubmission)
protocol of `ouroroboros-network`.

If the network connects only interested peers, _broadcast_ is essentially the
_multicast_ required by the protocol. If this is not the case, some addressing
scheme is required and _broadcast_ would be a waste of resources.

## Decision

* All messages emitted by a Hydra node through the Network component are _broadcasted_ to _all_ nodes in the network
* This implies the emitter shall itself receive the message

## Consequences

* The network layer is responsible for ensuring sent messages effectively
  reaches all nodes in the network. How this is achieved is left as an
  implementation detail, i.e. whether it uses relaying or not.
* We need to make sure all Head participants are connected to the same network.

