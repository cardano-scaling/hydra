---
slug: 17
title: |
  17. Use UDP protocol for Hydra networking
authors: []
tags: [Draft]
---

## Status

Draft

## Context

Current Hydra networking layer is based on [Ouroboros network framework](https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network-framework) networking stack which, among other features, provides:
1. An abstraction of stream-based duplex communication channels called a [Snocket](https://github.com/input-output-hk/ouroboros-network/blob/6c15a8093bac34091ad96af2b8b0d1f7fe54b732/ouroboros-network-framework/src/Ouroboros/Network/Snocket.hs),
2. A Multiplexing connection manager that manages a set of equivalent peers, maintains connectivity, and ensures diffusion of messages to/from all peers,
2. Typed protocols for expressing the logic of message exchanges as a form of _state machine_.

While it's been working mostly fine so far, the abstractions and facilities provided by this network layer are not well suited for Hydra Head networking. Some of the questions and shortcomings are discussed in a document on [Networking Requirements](/core-concepts/networking), and as the Hydra Head matures it seems time is ripe for overhauling current network implementation to better suite current and future Hydra Head networks needs.

## Decision

* Hydra Head nodes communicate by sending messages to other nodes using [UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) protocol

## Details

* _How do nodes know each other?_: This is unspecified by this ADR and left for future work, it is assumed that a Hydra node operator knows the IP:Port address of its peers before opening a Head with them
* _Are messages encrypted?_: This should probably be the case in order to ensure Heads' privacy but is also left for future work
* _How are nodes identified?_: At the moment they are identified by their IP:Port pair. As we implement more of the setup process from section 4 of the Hydra Head paper, we should identify nodes by some public key(hash) and resolve the actual IP:Port pair using some other mechanism

## Consequences

* Node's _HeadLogic_ handles lost, duplicates, and out-of-order messages using _retry_ and _timeout_ mechanisms
* Messages should carry a unique identifier, eg. source node and index
* Protocol, eg. messages format, is documented
