# Welcome

Hydra is a layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This is the user manual for `hydra-node`, an implementation of the Hydra Head protocol, as originally published [here](https://eprint.iacr.org/2020/299.pdf). The `hydra-node` interfaces the Cardano blockchain, connects to other `hydra-node`s on a dedicated overlay network, runs a simplified (coordinated) Hydra Head protocol, and provides an API to clients.

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

Now, without further ado, read more about the protocol [on the next page](./protocol-overview) or jump right in to [getting started using a local devnet](./getting-started).

[known-issues]: ./known-issues.md
[license]: https://github.com/input-output-hk/hydra/blob/master/LICENSE
