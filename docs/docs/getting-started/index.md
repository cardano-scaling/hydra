---
sidebar_position: 1
---

# Getting Started

Hydra is the layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This repository contains the proof-of-concept implementation for the Hydra [Head
protocol](https://eprint.iacr.org/2020/299.pdf). It is a developer preview that
we've put together, marching towards a production ready solution. It contains a
`hydra-node`, which runs a simplified (coordinated) Hydra Head protocol,
connects to other hydra-nodes, interfaces the Cardano blockchain and provides an
API to clients such as the included example terminal user interface `hydra-tui`.

:::warning Mainnet Availability

The Hydra Head protocol version 0.10.0 or newer is compatible with the Cardano
mainnet, which means it is possible to run a hydra-node on mainnet for testing
purposes.

Before running a `hydra-node` to take part in the Hydra Head protocol,
developers are strongly encouraged to review the [known issues][known-issues] in
the documentation in order to understand the current limitations and the
possible consequences.

By using Hydra Head protocol version 0.10.0 or newer, you understand the
protocol is in development and that use of the hydra-node on mainnet is entirely
at your own risk.

You also acknowledge and agree to have an adequate understanding of the risks
associated with use of the Hydra Head protocol version 0.10.0 or newer and that
all information and materials published, distributed or otherwise made available
on Hydra.Family and Hydra Github Repository is available on an ‘AS IS’ and ‘AS
AVAILABLE’ basis, without any representations or warranties of any kind. All
implied terms are excluded to the fullest extent permitted by law. For details,
see also sections 7, 8 and 9 of the [Apache 2.0 License][license].

:::

[known-issues]: ../known-issues
[license]: https://github.com/input-output-hk/hydra/blob/master/LICENSE

```mdx-code-block
import DocCardList from '@theme/DocCardList';
import {useCurrentSidebarCategory} from '@docusaurus/theme-common';

<DocCardList items={useCurrentSidebarCategory().items}/>
```
