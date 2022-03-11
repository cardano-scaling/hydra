# <p align="center">Hydra Head - Proof of Concept (POC)</p>

<div align="center">
  <p>A home to our colorful experiments and prototypes.</p>
  <a href='https://github.com/input-output-hk/hydra-poc/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
  <a href='https://github.com/input-output-hk/hydra-poc/pkgs/container/hydra-node'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/Docker?label=Docker&style=for-the-badge" /></a>
</div>

## :sunrise_over_mountains: Introduction

Hydra is the layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This repository contains the proof-of-concept implementation for the Hydra
[Head protocol](https://eprint.iacr.org/2020/299.pdf).  It is a developer
preview that we've put together, marching towards a production ready solution.
It outlines the basic architecture of a `hydra-node`, which runs a simplified
(coordinated) Hydra Head protocol, connects to other hydra-nodes, interfaces
the Cardano blockchain and provides an API to clients such as the included
terminal user interface `hydra-tui`.

> :warning: :warning: :warning:
> 
> This is still prototypical and exploratory work shared here for your
interest.
>
> It is NOT ready for production (yet).

## :rocket: Getting started

The quickest way to get a `hydra-node` running is to use our [docker
images](https://github.com/orgs/input-output-hk/packages?repo_name=hydra-poc).

```sh
docker pull ghcr.io/input-output-hk/hydra-node:latest
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

More information (including a demo) available on [user manual 📖](https://input-output-hk.github.io/hydra-poc/)!

## :rainbow: Features

Proof of concept:
- [x] Coordinated Hydra Head protocol
- [x] Single Head per hydra-node
- [x] Stubbed chain using external process
- [x] Network statically configured, direct TCP connections
- [x] WebSocket, message-based API Server
- [x] Terminal user interface client
- [x] Cardano-node integration via Direct connection

Later:
- [ ] Running on testnets and mainnet
- [ ] Persisted Head state
- [ ] Multiple Heads per hydra-node, managed via API
- [ ] Support for external wallets (e.g. hardware wallets)
- [ ] Optimistic Head closure and incremental de-/commit protocol extension
- [ ] Relay-capable, mesh network

---

<p align="center">
Thanks for visiting and enjoy :heart:!
</p>
