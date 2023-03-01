# <p align="center">Hydra :dragon_face:</p>

<div align="center">
  <p>Implementation of the Hydra scalability protocols.</p>
  <a href='https://github.com/input-output-hk/hydra/actions'><img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/hydra/ci.yaml?branch=master&label=Tests&style=for-the-badge" /></a>
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

> :warning: :warning: :warning:
>
> This is still prototypical and exploratory work shared here for your
interest.
>
> It is NOT ready for production (yet).

## :rocket: Getting started

The quickest way to get a `hydra-node` running is to use our [docker
images](https://github.com/orgs/input-output-hk/packages?repo_name=hydra).

```sh
docker pull ghcr.io/input-output-hk/hydra-node:0.9.0
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
