# <p align="center">Hydra :dragon_face:</p>


<div align="center">
  <p>Implementation of the Hydra scalability protocols.</p>
  <a href='https://github.com/input-output-hk/hydra/actions'><img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/hydra/ci-nix.yaml?branch=master&label=Tests&style=for-the-badge" /></a>
  <a href='https://github.com/input-output-hk/hydra/pkgs/container/hydra-node'><img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/hydra/docker.yaml?branch=master&label=Docker&style=for-the-badge" /></a>
</div>

## :sunrise_over_mountains: Introduction

Hydra is the layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This repository contains the proof-of-concept implementation for the Hydra [Head
protocol](https://eprint.iacr.org/2020/299.pdf). It is a developer preview that
we've put together, marching towards a production ready solution. It contains a
`hydra-node`, which runs a simplified (coordinated) Hydra Head protocol,
connects to other hydra-nodes, interfaces the Cardano blockchain and provides an
API to clients such as the included example terminal user interface `hydra-tui`.

:rotating_light: Mainnet Availability :rotating_light:

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

[known-issues]: https://hydra.family/head-protocol/docs/known-issues
[license]: ./LICENSE

## :rocket: Getting started

The quickest way to get a `hydra-node` running is to use our [docker
images](https://github.com/orgs/input-output-hk/packages?repo_name=hydra).

```sh
docker pull ghcr.io/input-output-hk/hydra-node
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

More information (including a demo) available on [user manual 📖](https://hydra.family/head-protocol/docs/getting-started)!

## :rainbow: Features

Proof of concept:
- [x] Coordinated Hydra Head protocol
- [x] Single Head per hydra-node
- [x] Network statically configured, direct TCP connections
- [x] WebSocket, message-based API Server
- [x] Terminal user interface client
- [x] Cardano-node integration via Direct connection
- [x] Running on testnet
- [x] Persisted Head state

Later:
- [ ] Audited and mainnet ready
- [ ] Multiple Heads per hydra-node, managed via API
- [ ] Support for external wallets (e.g. hardware wallets)
- [ ] Optimistic Head closure and incremental de-/commit protocol extension
- [ ] Relay-capable, mesh network

## :handshake: Contributing

The best way to contribute right now is to provide feedback. Give the
[demo](./demo) a test drive and have a look at our [documentation](https://hydra.family/head-protocol).
Should you have any questions, ideas or issues, we would like to hear from you:

- #ask-hydra on the IOG [Discord server](https://discord.gg/Qq5vNTg9PT)
- create a Github [Discussion](https://github.com/input-output-hk/hydra/discussions) or [Issue](https://github.com/input-output-hk/hydra/issues/new)
- or ask on Cardano [StackExchange](https://cardano.stackexchange.com/) using the `hydra` tag

When contributing to this project and interacting with others, please follow our [Contributing Guidelines](./CONTRIBUTING.md) and [Code of Conduct](./CODE-OF-CONDUCT.md).

---

<p align="center">
Thanks for visiting and enjoy :heart:!
</p>
