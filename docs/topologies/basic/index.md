---
sidebar_label: 'Basic Hydra head'
sidebar_position: 2
---

# Basic Hydra head

:::note

:hammer_and_wrench: This document is a work in progress.

:::

This document outlines the deployment architecture of a basic Hydra head. It serves as a foundational reference for other topologies discussed in this chapter and is illustrated below:

```mdx-code-block
<p align="center">
  <img
    src={require('./basic-hydra-head.jpg').default}
    alt="Basic Hydra Head"
    height={400}
  />
</p>
```

The basic setup of a Hydra head involves several `hydra-node`s, each connected to the Cardano network via a `cardano-node` (not depicted in the diagram). A Hydra client, such as `hydra-tui`, typically connects locally to a `hydra-node` to initiate a Hydra head using an off-chain network. The diagram displays two Hydra heads (colored blue and green) established between two distinct sets of `hydra-node`s. The lines in the diagram represent Hydra network connections, and the circles symbolize the Hydra head state and credentials, collectively referred to as a *Hydra head party*.

The diagram does not show multiple, logical Hydra heads operating concurrently within the same `hydra-node`. This capability, likely to be supported in the future, would facilitate the reuse of network connections between `hydra-node` processes.

Each head, whether blue or green, progresses independently and requires the endorsement of all respective *Hydra parties* within each head. For instance, the green head requires two signatures, while the blue head requires four.
