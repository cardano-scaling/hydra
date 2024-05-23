# Welcome

This section is a user manual for Hydra node operators. 

Hydra node is an implementation of the Hydra Head protocol, designed to enable fast and low-cost transactions by operating off-chain while leveraging the security of the main Cardano blockchain.

Decentralized applications (DApps), exchanges, and enterprise-level services can choose to run a Hydra node to participate in forming Hydra Heads – mini-ledgers that process transactions independently and in parallel, significantly enhancing throughput and reducing latency. 

To run a Hydra node, participants should:

* **Install and configure the node**: set up the Hydra node software, ensuring it is properly configured to connect with other Hydra nodes and the Cardano mainnet.
* **Participate in Hydra heads**: collaborate with other nodes to create and manage Hydra heads, enabling batch processing of transactions off-chain.
* **Synchronize with Cardano**: maintain synchronization with the main chain to ensure the security and finality of transactions processed by the Hydra heads.

Navigate through tutorials and documentation guides to get started. If you want to learn more about Hydra, see the [developer documentation section](https://hydra.family/head-protocol/docs/dev). If you're interested in building, see the [core concencepts section](https://hydra.family/head-protocol/core-concepts).

:::warning Mainnet Availability disclaimer

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
associated with use of the Hydra Head protocol version 0.10.0 or newer and that
all information and materials published, distributed or otherwise made available
on hydra.family and Hydra Github Repository is available on an ‘AS IS’ and ‘AS
AVAILABLE’ basis, without any representations or warranties of any kind. All
implied terms are excluded to the fullest extent permitted by law. For details,
see also sections 7, 8 and 9 of the [Apache 2.0 License][license].
:::