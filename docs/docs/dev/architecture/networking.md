# Networking

This page provides details about the Hydra networking layer, through which hydra
nodes exchange off-chain protocol messages. The off-chain protocol relies
heavily on the correct operation of the **multicast** abstraction (`broadcast`
in our fully connected topology here) in the way [it is
specified](../specification) and the following sections explain our realization
in the Hydra node implementation.

### Interface

Within a `hydra-node`, a `Network` component provides the capability to reliably
`broadcast` a message to the whole Hydra network. In turn, when a message is
received from the network, the `NetworkCallback` signals this by invoking
`deliver`. This interface follows reliable broadcast terminology of distributed
systems literature.

Given the way the [off-chain protocol is specified](../specification), the
`broadcast` abstraction required from the `Network` interface is a so-called
_uniform reliable broadcast_ with properties:

1. **Validity**: If a correct process p broadcasts a message m, then p eventually delivers m.
2. **No duplication**: No message is delivered more than once.
3. **No creation**: If a process delivers a message m with sender s, then m was
previously broadcast by process s.
4. **Agreement**: If a message m is delivered by some correct process, then m is
eventually delivered by every correct process.

See also Module 3.3 in [Introduction to Reliable and Secure Distributed
Programming](https://www.distributedprogramming.net) by Cachin et al, or
[Self-stabilizing Uniform Reliable Broadcast by Oskar
Lundström](https://arxiv.org/abs/2001.03244); or [atomic
broadcast](https://en.m.wikipedia.org/wiki/Atomic_broadcast) for an even
stronger abstraction.

### Topology

Currently, the `hydra-node` operates in a static, **fully connected** network
topology where each nodes connects to each other node and a message is broadcast
to all nodes. For this, we need to pass publicly reachable endpoints of *all
other nodes* via `--peer` options to each hydra node and *all links* must be
operational to achieve liveness.

Alternative implementations of a the `Network` interface could improve upon this
by enabling **mesh** topologies where messages are forwarded across links. This
would simplify configuration to only need to provide *at least one* `--peer`,
while *peer sharing* in such a network could still allow for redundant
connections and better fault tolerance.

### Authentication

The messages exchanged through the _Hydra networking_ layer between participants
are authenticated. Each message is
[signed](https://github.com/input-output-hk/hydra/issues/727) using the Hydra
signing key of the emitting party, which is identified by the corresponding
verification key. When a message with an unknown or incorrect signature is
received, it is dropped, and a notification is logged.

Currently, messages are not encrypted. If confidentiality is required, an
external mechanism must be implemented to prevent other parties from observing
the messages exchanged within a head.

### Fault tolerance

Although the Hydra protocol can only progress when nodes of all participants are
online and responsive, the network layer should still provide a certain level of
tolerance to crashes, transient connection problems and *non-byzantine* faults.

Concretely, this means that a _fail-recovery_ distributed systems model (again see Cachin et al) seems to fit these requirements best. This means, that processes may crash and later recover should still be able to participate in the protocol. Processes may forget what they did prior to crashing, but may use stable storage to persist knowledge. Links may fail and are _fair-loss_, where techniques to improve them to _stubborn_ or _perfect_ links likely will be required.

See also [this ADR](/adr/27) for a past discussion on making the network component resilient against faults.

## Investigations

### Network resilience

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
