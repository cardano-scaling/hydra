# <p align="center">Hydra Head Proof of Concept (POC)</p>

<div align="center">
  <p>A home to our colorful experiments and prototypes.</p>
  <a href='https://github.com/input-output-hk/hydra-poc/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
  <a href='https://github.com/input-output-hk/hydra-poc/pkgs/container/hydra-node'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/Docker?label=Docker&style=for-the-badge" /></a>
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
- [ ] Running on testnets and mainnet
- [ ] Persisted Head state
- [ ] Multiple Heads per hydra-node, managed via API
- [ ] Support for external wallets (e.g. hardware wallets)
- [ ] Optimistic Head closure and incremental de-/commit protocol extension
- [ ] Relay-capable, mesh network

## :rocket: Getting started

The quickest way to get a `hydra-node` running is to use our [docker
images](https://github.com/orgs/input-output-hk/packages?repo_name=hydra-poc).

```sh
docker pull ghcr.io/input-output-hk/hydra-node:latest
docker run --rm ghcr.io/input-output-hk/hydra-node --help
```

A full example scenario with three nodes connected off-chain and three Head
participants can be found in our [demo section](./demo).

## :question: Contributing

The best way to contribute right now is to provide feedback. Give the
[demo](./demo) a test drive and have a look at our [documentation](./docs).
Should you have any questions, ideas or issues, we would like to hear from you:

- #ask-hydra on the IOG [Discord server](https://discord.gg/Qq5vNTg9PT)
- create a Github [Discussion](https://github.com/input-output-hk/hydra-poc/discussions) or [Issue](https://github.com/input-output-hk/hydra-poc/issues/new)
- or ask on Cardano [StackExchange](https://cardano.stackexchange.com/) using the `hydra` tag

When contributing to this project and interacting with other contributors, please follow our [Code of Conduct](./CODE-OF-CONDUCT.md).

## :books: Documentation

Adding onto the information found here and the wiki there is some [technical
documentation](./docs), which is also published online at
https://input-output-hk.github.io/hydra-poc.

API Documentation is available for:
* The [WebSocket API](https://input-output-hk.github.io/json-schema-viewer/#/view?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/json-schemas/api.yaml)
* The [Log API](https://input-output-hk.github.io/json-schema-viewer/#/view?url=https://raw.githubusercontent.com/input-output-hk/hydra-poc/master/hydra-node/json-schemas/logs.yaml)

[Haddock](https://www.haskell.org/haddock/) documentation is also published for each package:
* [hydra-prelude](https://input-output-hk.github.io/hydra-poc/haddock/hydra-prelude/index.html)
* [hydra-cardano-api](https://input-output-hk.github.io/hydra-poc/haddock/hydra-cardano-api/index.html)
* [hydra-node](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/index.html)
    * [hydra-node tests](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/tests/index.html)
* [hydra-tui](https://input-output-hk.github.io/hydra-poc/haddock/hydra-tui/index.html)
* [hydra-plutus](https://input-output-hk.github.io/hydra-poc/haddock/hydra-plutus/index.html)
* [hydra-cluster](https://input-output-hk.github.io/hydra-poc/haddock/hydra-cluster/index.html)
* [merkle-patricia-tree](https://input-output-hk.github.io/hydra-poc/haddock/merkle-patricia-tree/index.html)
* [plutus-cbor](https://input-output-hk.github.io/hydra-poc/haddock/plutus-cbor/index.html)

## :wrench: Development

### Building & Testing

#### With nix

We provide a `shell.nix` to set up a development environment. So a simple call
to `nix-shell` should put everything in place for building, testing and
general development.

Make sure the following caches are listed in your `nix.conf` for a speedy setup:

```
substituters = https://cache.nixos.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Also, some of us use [direnv](https://direnv.net/) and
[nix-direnv](https://github.com/nix-community/nix-direnv) to automatically
import & cache the nix-shell environment into our favorite shell or editor.

Within the `nix-shell`, `cabal build` and `cabal test` should work as expected.

You can also use `nix-build` to build the project and all its executables. You
will find them in `result/bin/` after the build.

#### Without nix

1. Install basic Haskell development environment, for example using [ghcup](https://www.haskell.org/ghcup/install/). Hydra requires GHC 8.10.7 and a recent cabal (> 3.0).
2. Install various system dependencies
   On Debian-like:
   ```
   sudo apt install -y  build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
   sudo apt install -y  libz-dev liblzma-dev libzmq3-dev pkg-config libtool
   ```
   Do not confuse `lzma` with `liblzma-dev`, those are 2 existing package
3. Install [forked libsodium](https://github.com/input-output-hk/libsodium)
   ```
   git clone https://github.com/input-output-hk/libsodium
   cd libsodium/
   git checkout 66f017f16633f2060db25e17c170c2afa0f2a8a1
   ./autogen.sh
   ./configure
   make && sudo make install
   ```
4. Build and test everything:
   ```
   cabal build all && cabal test all
   ```

### Releasing

* During development
  + Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/)
  + Bump `UNRELEASED` version in `CHANGELOG.md` according to [semver](https://semver.org/)

* To release
  + Check version to be released is also correct in all `hydra-` software components, e.g. `.cabal` files
  + Replace `UNRELEASED` with a date in [ISO8601](https://en.wikipedia.org/wiki/ISO_8601)
  + Create a signed, annotated git tag of the version: `git tag -as <version>`
  + (ideally) Use the released changes as annotation

* Note on non `hydra-` components
  + This monorepo contains also other packages which may be released independently
  + Individual `CHANGELOG.md` files are used, e.g. `merkle-patricia-tree/CHANGELOG.md`
  + Signed, annotated git tag with full package name + version shall be used,
    e.g. `git tag -as merkle-patricia-tree-0.1.0`
