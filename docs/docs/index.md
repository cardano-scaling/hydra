# Welcome

This section is a manual for users interested in running a Hydra node.

Hydra node is an implementation of the Hydra Head protocol, designed to enable fast and low-cost transactions by operating off-chain while leveraging the security of the main Cardano blockchain.

Decentralized applications (DApps), exchanges, and enterprise-level services can choose to run a Hydra node to participate in forming Hydra heads – mini-ledgers that process transactions independently and in parallel, significantly enhancing throughput and reducing latency.

The `hydra-node` interfaces with the Cardano blockchain, connects to other `hydra-nodes` on a dedicated overlay network, runs a simplified (coordinated) Hydra Head protocol, and provides an API for clients.

:::warning Mainnet availability disclaimer

The Hydra Head protocol version 0.10.0 or newer is compatible with the Cardano
mainnet, which means it is possible to run a `hydra-node` on mainnet using real
funds.

Before running a `hydra-node` to take part in the Hydra Head protocol,
developers are strongly encouraged to review the [known issues][known-issues] in
the documentation in order to understand the current limitations and the
possible consequences.

By using Hydra Head protocol version 0.10.0 or newer, you understand the
protocol is in development and that use of the `hydra-node` on mainnet is
entirely at your own risk.

You also acknowledge and agree to have an adequate understanding of the risks
associated with the use of the Hydra Head protocol version 0.10.0 or newer and that
all information and materials published, distributed or otherwise made available
on hydra.family and Hydra GitHub repository is available on an ‘AS IS’ and ‘AS
AVAILABLE’ basis, without any representations or warranties of any kind. All
implied terms are excluded to the fullest extent permitted by law. For details,
see also sections 7, 8 and 9 of the [Apache 2.0 License][license].
:::

Now, without further ado, you can learn more about the protocol by visiting the [protocol overview page](./docs/protocol-overview) or directly dive into [getting started using a local devnet](./docs/getting-started).

[known-issues]: ./known-issues.md
[license]: https://github.com/cardano-scaling/hydra/blob/master/LICENSE
