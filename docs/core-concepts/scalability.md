---
sidebar_position: 2
---

# Scalability

Any decentralized system is fundamentally limited in its scalability. This also
applies to blockchains and is rooted in the fact that security is achieved
through massive global replication of transactions. The more decentralized the
system is, the more time and other resources are used up to process
transactions. This is also often called the blockchain trilemma, where
_decentralization_, _security_ and _scalability_ counter-act each other.

Even though Cardano uses a very efficient consensus algorithm, the fact that
it's distributed globally between thousands of block-producing nodes (with quite
a low bar in system requirements) will have it create a block on average every
20 seconds.

These limitations ultimately mean that any state can only change in discrete
steps of this duration. Besides, given peak hours on the blockchain when there
is a transaction queue, the time required to settle and confirm a transaction
might also be higher, further increasing the effective settlement time. This is
because your transaction might not be added to the next block, but the second or
even third one coming.

## Vertical and horizontal scaling

In general, there are two ways of scaling systems: **vertical** and
**horizontal**. Both try to increase resources available to the system to
achieve better overall performance.

For vertical scaling, the performance of the system is increased by adding
resources available to the already existing instances of the system. For
Cardano, this practically means increasing the block size or reducing the block
time. This is a great way to initially scale the system, but is ultimately
limited by network latency and processing power of block-producing nodes. To
ensure the security of the system, each block has roughly 5 seconds to propagate
through the network, which includes relaying and validating it through multiple
hops. At some point, driving the system requirements up will also reduce the
level of decentralization, as less individuals will be able to run such a node.

Scaling a system horizontally, means to increase performance of the overall
system by adding more individual instances alongside each other. Practically,
this could mean that besides the main chain, multiple side chains are spun up
that do the same thing, each "x" seconds all instances create a block. Unlike
vertical scaling, horizontal scaling does not have a direct limit, there can be
many side chains that are connected to the layer one. A major downside to this
way of scaling is that any state of the system is split into multiple pieces.
Each instance is blind to what is happening on the other instances and moving
state between instances results requires additional work (the state is sharded).

![Horizontal & Vertical scaling](./horizontal-vertical-scaling.png)

## State channels

The Hydra Head protocol is a form of state channel and can be mostly classified
as a horizontal scaling solution. While multiple instances of them can be
deployed to off-load and increase the scalability of the overall system, it
provides for a flexible way to decide on the level of decentralization of each
instance and provides a mostly frictionless way to transfer state between the
mainchain and the individual Hydra heads (through it _isomorphic_ nature).

![State Channel](./state-channel.png)

State channels allow to take parts of the state from the layer one blockchain
and validate its progress elsewhere between only those parties who are concerned
about this state. Then, after this computation is done, the parties return the
final state on which all parties agree back to layer one. This construction
means that the 20 second block time limit no longer applies and state can be
evolved as fast as only the involved parties approve.

In conclusion, Hydra Head will allow operators to strike the fundamental
trade-off between decentralization, security and scalability different than the
underlying blockchain to serve the needs of individual applications.
