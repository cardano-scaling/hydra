---
sidebar_label: 'Basic Hydra Head'
sidebar_position: 2
---

# Basic Hydra Head

:hammer_and_wrench: This document is a work in progress

This document outlines the deployment architecture of a basic Hydra Head. It serves as a foundational reference for other topologies discussed in this chapter and is illustrated below:

```mdx-code-block
<p align="center">
  <img
    src={require('./basic-hydra-head.jpg').default}
    alt="Basic Hydra Head"
    height={400}
  />
</p>
```

The basic setup of a Hydra Head involves several `hydra-node`s, each connected to the Cardano network via a `cardano-node` (not depicted in the image). A Hydra client, such as `hydra-tui`, typically connects locally to a `hydra-node` to initiate a Hydra Head using an off-chain network. The diagram displays two Hydra Heads (colored blue and green) established between two distinct sets of `hydra-node`s. The lines in the diagram represent Hydra network connections, and the circles symbolize the Hydra Head state and credentials, collectively referred to as a *Hydra Head Party*.

The diagram does not show multiple, logical Hydra Heads operating concurrently within the same `hydra-node`. This capability, likely to be supported in the future, would facilitate the reuse of network connections between `hydra-node` processes.

Each head, whether blue or green, progresses independently and requires the endorsement of all respective *Hydra Parties* within each head. For instance, the green head requires 2 signatures, while the blue head requires 4.