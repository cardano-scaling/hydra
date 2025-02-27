# Networking

This page provides details about the Hydra networking layer, through which hydra nodes exchange off-chain protocol messages. The off-chain protocol relies heavily on the correct operation of the **multicast** abstraction (`broadcast` in our fully connected topology here) in the way [it is specified](../specification) and the following sections explain our realization in the Hydra node implementation.

## Interface

Within a `hydra-node`, a `Network` component provides the capability to reliably `broadcast` a message to the whole Hydra network. In turn, when a message is received from the network, the `NetworkCallback` signals this by invoking `deliver`. This interface follows reliable broadcast terminology of distributed systems literature.

Given the way the [off-chain protocol is specified](../specification), the `broadcast` abstraction required from the `Network` interface is a so-called _uniform reliable broadcast_ with properties:

1. **Validity**: If a correct process p broadcasts a message m, then p eventually delivers m.
2. **No duplication**: No message is delivered more than once.
3. **No creation**: If a process delivers a message m with sender s, then m was previously broadcast by process s.
4. **Agreement**: If a message m is delivered by some correct process, then m is eventually delivered by every correct process.

See also Module 3.3 in [Introduction to Reliable and Secure Distributed Programming](https://www.distributedprogramming.net) by Cachin et al, or [Self-stabilizing Uniform Reliable Broadcast by Oskar Lundström](https://arxiv.org/abs/2001.03244); or [atomic broadcast](https://en.m.wikipedia.org/wiki/Atomic_broadcast) for an even stronger abstraction.

## Fault model

Although the Hydra protocol can only progress when nodes of all participants are online and responsive, the network layer should still provide a certain level of tolerance to crashes, transient connection problems and *non-byzantine* faults.

Concretely, this means that a _fail-recovery_ distributed systems model (again see Cachin et al) seems to fit these requirements best. This means, that processes may crash and later recover should still be able to participate in the protocol. Processes may forget what they did prior to crashing, but may use stable storage to persist knowledge. Links may fail and are _fair-loss_, where techniques to improve them to _stubborn_ or _perfect_ links likely will be required.

See also [ADR 27 (superseded)](/adr/27) and [ADR 32](/adr/32) for more context on required properties of the network layer and attempts to achieve fault tolerance.

## Topology

Currently, the `hydra-node` operates in a static, **fully connected** network topology where each node connects to each other node and a message is broadcast to all nodes. For this, we need to pass publicly reachable endpoints of *all other nodes* via `--peer` options to each hydra node and *all links* must be operational to achieve liveness[^1].

[^1]: This is not entirely true anymore as the `etcd` based implementation requires only a connection to the majority of the cluster.

Future implementations of a the `Network` interface could improve upon this by enabling **mesh** topologies where messages are forwarded across links. This would simplify configuration to only need to provide *at least one* `--peer`, while *peer sharing* in such a network could still allow for redundant connections and better fault tolerance.

## Authentication

The messages exchanged through the _Hydra networking_ layer between participants are authenticated. Each message is [signed](https://github.com/cardano-scaling/hydra/issues/727) using the Hydra signing key of the emitting party, which is identified by the corresponding verification key. When a message with an unknown or incorrect signature is received, it is dropped, and a notification is logged.

## Encryption

Currently, communication is _not_ encrypted.

If confidentiality is required, two options exist: encrypt individual messages (symmetric block cipher initiated through a key exchange) or establish encrypted channels between peers (e.g. using TLS).

## Implementation

The current implementation of the [network stack](pathname:///haddock/hydra-node/Hydra-Node-Network.html) consists of two components:

- [`Authenticate`](pathname:///haddock/hydra-node/Hydra-Network-Authenticate.html) component that signs all outgoing and verifies incoming messages using the Hydra snapshot key pair
- [`Etcd`](pathname:///haddock/hydra-node/Hydra-Network-Etcd.html) component that re-uses [etcd](https://etcd.io/) to implement a reliable `broadcast` primitive

See [ADR 32](/adr/32) for a rationale of this approach.

### Previous network stack

In the past we had a "hand-rolled" network stack to implement reliable broadcast in a fully connected network topology. 

- Hydra nodes form a network of pairwise connected *peers* using point-to-point TCP connections that are expected to remain active at all times
- Using [ouroboros-framework](https://github.com/input-output-hk/ouroboros-network/) to establish point-to-point stream-based connections in a simple `FireForget` protocol (stateless, just sending messages).
- Nodes keep a "valency" matching the number of other peers and broadcast messages to everyone.
- Implement basic _failure detection_ through heartbeats and monitoring exchanged messages.
- Reliable broadcast is (attempted to be) realized by storing outgoing messages, acknowledgments via a vector clock transmitted on each message, and message resending if clock is out-of-sync.

However, it was not as reliable as we hoped (in theory it should have worked) and we pivoted to the current, etcd-based implementation.

### Gossip diffusion network (idea)

The following diagram illustrates an ideated pull-based messaging system for Hydra. This would follow the design of the Cardano network stack:

![Hydra pull-based network](./hydra-pull-based-network.jpg)

This was not (yet) pursued as it does not fit the [broadcast interface](#interface) as required by the Hydra Head protocol design. However, we think it _should_ be possible to represent the off-chain protocol using pull-based semantics and follow this approach.

## Network resilience testing

In August 2024 we added some network resilience tests, implemented as a GitHub
action step in [network-test.yaml](https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml).

The approach is to use [Pumba](https://github.com/alexei-led/pumba) to inject
networking faults into a docker-based setup. This is effective, because of the
[NetEm](https://srtlab.github.io/srt-cookbook/how-to-articles/using-netem-to-emulate-networks.html)
capability that allows for very powerful manipulation of the networking stack
of the containers.

Initially, we have set up percentage-based loss on some very specific
scenarios; namely a three-node setup between `Alice`, `Bob` and `Carol`.

With this setup, we tested the following scenarios:

- Three nodes, 900 transactions ("scaling=10"):
  - 1% packet loss to both peers: ✅ Success
  - 2% packet loss to both peers: ✅ Success
  - 3% packet loss to both peers: ✅ Success
  - 4% packet loss to both peers: ✅ Success
  - 5% packet loss to both peers: Sometimes works, sometimes fails
  - 10% packet loss to both peers: Sometimes works, sometimes fails
  - 20% packet loss to both peers: ❌Failure

- Three nodes, 4500 transactions ("scaling=50"):
  - 1% packet loss to both peers: ✅ Success
  - 2% packet loss to both peers: ✅ Success
  - 3% packet loss to both peers: ✅ Success
  - 4% packet loss to both peers: Sometimes works, sometimes fails
  - 5% packet loss to both peers: Sometimes works, sometimes fails
  - 10% packet loss to both peers: ❌Failure
  - 20% packet loss to both peers: ❌Failure

"Success" here means that _all_ transactions were processed; "Failure" means
one or more transactions did not get confirmed by all participants within a
particular timeframe.

The main conclusion here is ... there's a limit to the amount of packet loss
we can sustain, it's related to how many transactions we are trying to send
(naturally, [given the percent of failure is per
 packet](http://www.voiptroubleshooter.com/indepth/burstloss.html).)

You can keep an eye on the runs of this action here: [Network fault
tolerance](https://github.com/cardano-scaling/hydra/actions/workflows/network-test.yaml).

The main things to note are:

- Overall, the CI job will succeed even if every scenario fails. This is,
  ultimately, due to a bug in [GitHub
  actions](https://github.com/actions/runner/issues/2347) that prevents one
  from declaring an explicit pass-or-fail expectation per scenario. The impact
  is that you should manually check this job on each of your PRs.
- It's okay to see certain configurations fail, but it's certainly not
  expected to see them _all_ fail; certainly not the zero-loss cases. Anything
  that looks suspcisious should be investigated.
