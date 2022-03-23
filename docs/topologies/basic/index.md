---
sidebar_label: 'Basic Hydra Head'
sidebar_position: 2
---

# Basic Hydra Head

:hammer_and_wrench: This document is a work in progress

This document explains the deployment architecture of a basic Hydra Head. It
serves as a reference to other topologies discussed in this chapter and is
schematically depicted below:

```mdx-code-block
<p align="center">
  <img
    src={require('./basic-hydra-head.jpg').default}
    alt="Basic Hydra Head"
    height={400}
  />
</p>
```

The basic setup of a Hydra Head consists of several `hydra-node`s, each connected to the Cardano network through a (not pictured) `cardano-node`. A Hydra client (like the `hydra-tui`) would connect, usually via a local connection, to a `hydra-node` to open a Hydra Head using an off-chain network. The picture shows two Hydra Heads (blue and green) opened between two distinct sets of `hydra-node`s, where the lines indicate Hydra network connections and circles represent a Hydra Head state and credentials - summarized by a socalled *Hydra Head Party*. 

Not shown in the picture are multiple, logical Hydra Heads being open in the same `hydra-node`, which is very likely something we will support eventually and allow for re-using network connections between `hydra-node` processes.

Each head, the blue or green one, can progress independently and requires a signature of all the respective *Hydra Parties* of each head. That is, 2 signatures in the green Head and 4 signatures in the blue Head.
