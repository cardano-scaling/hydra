# <p align="center">Hydra Proof of Concept (POC)</p>

<div align="center">
  <p>A home to our colorful experiments and prototypes.</p>
  <a href='https://github.com/input-output-hk/hydra-poc/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
  <a href='https://hub.docker.com/r/inputoutput/hydra/tags'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/Docker?label=Docker&style=for-the-badge" /></a>
</div>

## :sunrise_over_mountains: Introduction

Hydra is the layer-two scalability solution for Cardano, which aims to increase
the speed of transactions (low latency, high throughput) and minimize
transaction cost.

This repository contains the proof-of-concept implementation the Hydra
engineering team has put together during exploration and can be considered a
"developer preview". It outlines the basic architecture of a `hydra-node`, which
runs a simplified (coordinated) [Hydra Head
protocol](https://eprint.iacr.org/2020/299.pdf), connects to other hydra-nodes,
interfaces the Cardano blockchain and provides an API to clients such as the
included terminal user interface `hydra-tui`.

On the Cardano Summit 2021 we talked about the overall vision, how the Hydra Head
works and showed a [demo :movie_camera:](https://summit.cardano.org/sessions/hydra-the-multi-headed-scalability-protocol).

:warning: This is still prototypical and exploratory work shared here for your
interest - it is NOT ready for production (yet) :warning:

Please also note that as we did develop this while in "move fast, break things"
experimentation mode, the code quality seen around here may not always represent
our best practices and you will find many code smells and dirty hacks.

Thanks for visiting and enjoy!

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
- [ ] Persisted Head state
- [ ] Multiple Heads per hydra-node, managed via API
- [ ] Support for external wallets (e.g. hardware wallets)
- [ ] Optimistic Head closure and incremental de-/commit protocol extension
- [ ] Relay-capable, mesh network

## :rocket: Getting started

The quickest way to get a `hydra-node` running is to use our [docker
images](https://hub.docker.com/r/inputoutput/hydra/tags).

```sh
docker pull inputoutput/hydra:hydra-node-latest
docker run --rm inputoutput/hydra:hydra-node-latest --help
```

First, let's prepare and start an ad-hoc, single `cardano-node` devnet using our
configuration. Note that this will create a `devnet` directory in your current
working directory:

```sh
./demo/prepare-devnet.sh
docker run -d --name cardano-node --network host \
  -v $PWD/devnet:/data \
  -v node-ipc:/ipc \
  -e CARDANO_BLOCK_PRODUCER=true \
  inputoutput/cardano-node run \
    --config /data/cardano-node.json \
    --topology /data/topology.json \
    --database-path /data/db \
    --socket-path /ipc/node.socket \
    --shelley-operational-certificate /data/credentials/opcert1.cert \
    --shelley-kes-key /data/credentials/delegate1.kes.skey \
    --shelley-vrf-key /data/credentials/delegate1.vrf.skey
```

In the POC, a `hydra-node` can only participate in a single Head and thus needs
a single `--hydra-signing-key`. For now, we use simplified keys for easier
debugging. To generate such a new key:

```sh
head -c8 /dev/random > devnet/alice-hydra.sk
```

Then, we can connect the `hydra-node` to the already running `cardano-node` and
attach a `hydra-tui` client:

```sh
docker run -d --name hydra-node --network host \
  -v $PWD/devnet:/data \
  -v node-ipc:/ipc \
  inputoutput/hydra:hydra-node-latest \
    --hydra-signing-key /data/alice-hydra.sk \
    --cardano-signing-key /data/credentials/alice.sk \
    --network-magic 42 \
    --node-socket ipc/node.socket

docker run --rm -it --network host inputoutput/hydra:hydra-tui-latest
```

Now you should see the terminal user interface connected to your node and you can initialize a Head, commit (generated) funds to it, send transactions and close the Head using the Commands/Hotkeys shown. Admittedly, as there are no other participants, this is a bit pointless. For a more interesting scenario with three nodes connected off-chain and three Head participants, check out our [demo section](./demo).

## :question: Contributing

The best way to contribute right now is to provide feedback. Give the
[demo](./demo) a test drive and have a look at our [documentation](./docs).
Should you have any questions, ideas or issues, we would like to hear from you:

- #ask-hydra on the IOG [Discord server](https://discord.gg/Qq5vNTg9PT)
- create a Github [Discussion](https://github.com/input-output-hk/hydra-poc/discussions) or [Issue](https://github.com/input-output-hk/hydra-poc/issues/new)
- or ask on Cardano [StackExchange](https://cardano.stackexchange.com/) using the `hydra` tag

When contributing to this project and interacting with other contributors, please follow our [Code of Conduct](./CODE-OF-CONDUCT.md).

## :wrench: Development

See the [Wiki](https://github.com/input-output-hk/hydra-poc/wiki/For-maintainers)

## :books: Documentation

Adding onto the information found here and the wiki there is some [technical
documentation](./docs), which is also published online at
https://input-output-hk.github.io/hydra-poc.

API Documentation is available for:
* The [WebSocket API](https://input-output-hk.github.io/json-schema-viewer/#/?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/json-schemas/api.yaml)
* The [Log API](https://input-output-hk.github.io/json-schema-viewer/#/?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/json-schemas/logs.yaml)

[Haddock](https://www.haskell.org/haddock/) documentation is also published for each package:
* [hydra-prelude](https://input-output-hk.github.io/hydra-poc/haddock/hydra-prelude/index.html)
* [hydra-node](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/index.html)
* [hydra-tui](https://input-output-hk.github.io/hydra-poc/haddock/hydra-tui/index.html)
* [hydra-plutus](https://input-output-hk.github.io/hydra-poc/haddock/hydra-plutus/index.html)
* [local-cluster](https://input-output-hk.github.io/hydra-poc/haddock/local-cluster/index.html)
* [merkle-patricia-tree](https://input-output-hk.github.io/hydra-poc/haddock/merkle-patricia-tree/index.html)
